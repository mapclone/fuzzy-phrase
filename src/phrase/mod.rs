pub mod util;
pub mod query;

use std::io;
#[cfg(feature = "mmap")]
use std::path::Path;

use fst;
use fst::IntoStreamer;
use fst::raw::{CompiledAddr, Node, Fst, Builder, Output};
use byteorder::{BigEndian, ReadBytesExt};

use self::util::{word_ids_to_key};
use self::util::PhraseSetError;
use self::query::QueryWord;

#[cfg(test)] mod tests;

type WordKey = [u8; 3];

pub struct PhraseSet(Fst);

/// PhraseSet is a lexicographically ordered set of phrases.
///
/// Phrases are sequences of words, where each word is represented as an integer. The integers
/// correspond to FuzzyMap values. Due to limitations in the fst library, however, the integers are
/// encoded as a series of 3 bytes.  For example, the three-word phrase "1## Main Street" will be
/// represented over 9 transitions, with one byte each.
///
/// | tokens  | integers  | three_bytes   |
/// |---------|-----------|---------------|
/// | 100     | 21        | [0,   0,  21] |
/// | main    | 457       | [0,   1, 201] |
/// | street  | 109821    | [1, 172, 253] |
///
impl PhraseSet {
    // Find a sequence of words in the phrase graph (either as a complete phrase entry or a prefix)
    pub fn lookup(&self, phrase: &[QueryWord]) -> PhraseSetLookupResult {
        let fst = &self.0;
        let mut node = fst.root();
        let mut output = Output::zero();
        for word in phrase {
            match word {
                QueryWord::Full { key, .. } => {
                    for b in key.into_iter() {
                        node = if let Some(i) = node.find_input(*b) {
                            let t = node.transition(i);
                            output = output.cat(t.out);
                            fst.node(t.addr)
                        } else {
                            return PhraseSetLookupResult::NotFound;
                        }
                    }
                },
                QueryWord::Prefix { key_range, .. } => {
                    match self.matches_prefix_range(
                        node.addr(),
                        output,
                        *key_range
                    ) {
                        WordPrefixMatchResult::Found(match_state) => {
                            // we can return and stop looping -- the prefix is at the end
                            return PhraseSetLookupResult::Found { fst, match_state: PhraseSetMatchState::EndsInPrefix(match_state) };
                        },
                        WordPrefixMatchResult::NotFound => {
                            return PhraseSetLookupResult::NotFound;
                        }
                    }
                },
            }
        }
        PhraseSetLookupResult::Found { fst, match_state: PhraseSetMatchState::EndsInFullWord { node, output } }
    }

    /// Recursively explore the phrase graph looking for combinations of candidate words to see
    /// which ones match actual phrases in the phrase graph
    ///
    /// This function takes as input a list of "word slots," for which one or more spelling
    /// variants of the same input word may be present. This variant will match phrases of exactly
    /// the same length as the number of slots in the input; it doesn't do prefix matching.
    pub fn match_combinations(
        &self,
        word_possibilities: &[Vec<QueryWord>],
        max_phrase_dist: u8
    ) -> Result<Vec<Combination>, PhraseSetError> {
        // this is just a thin wrapper around a private recursive function, with most of the
        // arguments prefilled
        let fst = &self.0;
        let root = fst.root();
        let mut out: Vec<Combination> = Vec::new();
        self.exact_recurse(word_possibilities, 0, &root, max_phrase_dist, Vec::new(), Output::zero(), &mut out)?;
        Ok(out)
    }

    fn exact_recurse(
        &self,
        possibilities: &[Vec<QueryWord>],
        position: usize,
        node: &Node,
        budget_remaining: u8,
        words_so_far: Vec<QueryWord>,
        output_so_far: Output,
        out: &mut Vec<Combination>,
    ) -> Result<(), PhraseSetError> {
        let fst = &self.0;

        for word in possibilities[position].iter() {
            let (key, edit_distance) = match word {
                QueryWord::Full { key, edit_distance, .. } => (*key, *edit_distance),
                _ => return Err(PhraseSetError::new(
                    "The query submitted has a QueryWord::Prefix. This function only accepts QueryWord:Full"
                )),
            };
            if edit_distance > budget_remaining {
                break
            }

            // can we find the next word from our current position?
            let mut found = true;
            // make a mutable copy to traverse
            let mut search_node = node.to_owned();
            let mut incr_output = Output::zero();
            for b in key.into_iter() {
                if let Some(i) = search_node.find_input(*b) {
                    let trans = search_node.transition(i);
                    incr_output = incr_output.cat(trans.out);
                    search_node = fst.node(trans.addr);
                } else {
                    found = false;
                    break;
                }
            }

            // only recurse or add a result if the current word is in the graph in this position
            if found {
                let mut rec_so_far = words_so_far.clone();
                rec_so_far.push(word.clone());
                if position < possibilities.len() - 1 {
                    self.exact_recurse(
                        possibilities,
                        position + 1,
                        &search_node,
                        budget_remaining - edit_distance,
                        rec_so_far,
                        output_so_far.cat(incr_output),
                        out,
                    )?;
                } else {
                    // if we're at the end of the line, we'll only keep this result if it's final
                    if search_node.is_final() {
                        let final_output = output_so_far.cat(incr_output).cat(search_node.final_output());
                        out.push(Combination {
                            phrase: rec_so_far,
                            output_range: (final_output, final_output)
                        });
                    }
                }
            }
        }
        Ok(())
    }

    /// Recursively explore the phrase graph looking for combinations of candidate words to see
    /// which ones match prefixes of actual phrases in the phrase graph.
    ///
    /// As above, it's a list of word slots, but the last one might be a range of word IDs
    /// encompassing all words that start with a given prefix. The outputs it matches will be at
    /// least as long as the number of slots in the input, but maybe longer, and it may match
    /// either phrase prefixes that end in a word prefix (so, prefixes that start with some number
    /// of whole words plus a partial word) or phrase prefixes comprised only of whole words (so.
    /// phrases that start with a set of whole words followed by additional whole words).
    pub fn match_combinations_as_prefixes(
        &self,
        word_possibilities: &[Vec<QueryWord>],
        max_phrase_dist: u8
    ) -> Result<Vec<Combination>, PhraseSetError> {
        // this is just a thin wrapper around a private recursive function, with most of the
        // arguments prefilled
        let fst = &self.0;
        let root = fst.root();
        let mut out: Vec<Combination> = Vec::new();
        self.prefix_recurse(word_possibilities, 0, &root, max_phrase_dist, Vec::new(), Output::zero(), &mut out)?;
        Ok(out)
    }

    fn prefix_recurse(
        &self,
        possibilities: &[Vec<QueryWord>],
        position: usize,
        node: &Node,
        budget_remaining: u8,
        words_so_far: Vec<QueryWord>,
        output_so_far: Output,
        out: &mut Vec<Combination>,
    ) -> Result<(), PhraseSetError> {
        let fst = &self.0;

        for word in possibilities[position].iter() {
            match word {
                QueryWord::Full { key, edit_distance, .. } => {
                    if *edit_distance > budget_remaining {
                        break
                    }

                    let mut found = true;
                    // make a mutable copy to traverse
                    let mut search_node = node.to_owned();
                    let mut incr_output = Output::zero();
                    for b in key.into_iter() {
                        if let Some(i) = search_node.find_input(*b) {
                            let trans = search_node.transition(i);
                            incr_output = incr_output.cat(trans.out);
                            search_node = fst.node(trans.addr);
                        } else {
                            found = false;
                            break;
                        }
                    }

                    // only recurse or add a result if we the current word is in the graph in
                    // this position
                    if found {
                        let mut rec_so_far = words_so_far.clone();
                        rec_so_far.push(word.clone());
                        if position < possibilities.len() - 1 {
                            self.prefix_recurse(
                                possibilities,
                                position + 1,
                                &search_node,
                                budget_remaining - edit_distance,
                                rec_so_far,
                                output_so_far.cat(incr_output),
                                out,
                            )?;
                        } else {
                            let range = (PhraseSetMatchState::EndsInFullWord { node: search_node, output: output_so_far.cat(incr_output) }).prefix_range(fst);
                            out.push(Combination {
                                phrase: rec_so_far,
                                output_range: range
                            });
                        }
                    }
                },
                QueryWord::Prefix { key_range, .. } => {
                    if let WordPrefixMatchResult::Found(state) = self.matches_prefix_range(
                        node.addr(),
                        output_so_far,
                        *key_range
                    ) {
                        // presumably the prefix is at the end, so we don't need to consider the
                        // possibility of recursing, just of being done
                        let mut rec_so_far = words_so_far.clone();
                        rec_so_far.push(word.clone());
                        let range = (PhraseSetMatchState::EndsInPrefix(state)).prefix_range(fst);
                        out.push(Combination {
                            phrase: rec_so_far,
                            output_range: range
                        });
                    }
                },
            }
        }
        Ok(())
    }

    /// This variant searches the phrase graph given a query, and looks for phrases in the graph
    /// that are included anywhere in the query, or start with a word sequence at the end of the
    /// query.
    ///
    /// So, for example, for the input "100 main street new y", it might validly find a phrase
    /// "100 main street" (a full phrase contained within the query), and it might also match
    /// "new york" (a phrase that starts with a sequence at the end of the query). Prefix matching
    /// at the end of the query is optional, and controlled by the `ends_in_prefix` parameter.
    pub fn match_combinations_as_windows(
        &self,
        word_possibilities: &[Vec<QueryWord>],
        max_phrase_dist: u8,
        ends_in_prefix: bool
    ) -> Result<Vec<CombinationWindow>, PhraseSetError> {
        // this is just a thin wrapper around a private recursive function, with most of the
        // arguments prefilled
        let fst = &self.0;
        let root = fst.root();
        let mut out: Vec<CombinationWindow> = Vec::new();
        self.window_recurse(word_possibilities, 0, &root, max_phrase_dist, ends_in_prefix, Vec::new(), Output::zero(), &mut out)?;
        Ok(out)
    }

    fn window_recurse(
        &self,
        possibilities: &[Vec<QueryWord>],
        position: usize,
        node: &Node,
        budget_remaining: u8,
        ends_in_prefix: bool,
        words_so_far: Vec<QueryWord>,
        output_so_far: Output,
        out: &mut Vec<CombinationWindow>,
    ) -> Result<(), PhraseSetError> {
        let fst = &self.0;

        // This function can reach four different states in which it might produce output,
        // described individually below
        for word in possibilities[position].iter() {
            match word {
                QueryWord::Full { key, edit_distance, .. } => {
                    if *edit_distance > budget_remaining {
                        break
                    }

                    let mut found = true;
                    // make a mutable copy to traverse
                    let mut search_node = node.to_owned();
                    let mut incr_output = Output::zero();
                    for b in key.into_iter() {
                        if let Some(i) = search_node.find_input(*b) {
                            let trans = search_node.transition(i);
                            incr_output = incr_output.cat(trans.out);
                            search_node = fst.node(trans.addr);
                        } else {
                            found = false;
                            break;
                        }
                    }
                    // at this stage, incr_output will be the additional output state beyond
                    // what it was at the start of this function, based on having traversed one
                    // particular path from the intput node

                    // only recurse or add a result if we the current word is in the graph in
                    // this position
                    if found {
                        // we want to add a result if we're at the end OR if we've hit a final
                        // node OR we're at the end of the phrase
                        let mut rec_so_far = words_so_far.clone();
                        rec_so_far.push(word.clone());
                        if position < possibilities.len() - 1 {
                            if search_node.is_final() {
                                let final_output = output_so_far.cat(incr_output).cat(search_node.final_output());
                                // possibility number 1: we're not at the end of our input, but
                                // we've seen an entire phrase represented by input we've seen so
                                // far -- we've reached a final node in the graph
                                out.push(CombinationWindow {
                                    phrase: rec_so_far.clone(),
                                    output_range: (final_output, final_output),
                                    ends_in_prefix: false
                                });
                            }
                            self.window_recurse(
                                possibilities,
                                position + 1,
                                &search_node,
                                budget_remaining - edit_distance,
                                ends_in_prefix,
                                rec_so_far,
                                output_so_far.cat(incr_output),
                                out,
                            )?;
                        } else {
                            // if we're at the end, require final node unless autocomplete is on
                            if ends_in_prefix {
                                let range = (PhraseSetMatchState::EndsInFullWord { node: search_node, output: output_so_far.cat(incr_output) }).prefix_range(fst);
                                // possibility number 2: we *are* at the end of our input, and are
                                // doing prefix matching, so we're okay returning whatever partial
                                // phrase we happen to have found so far
                                out.push(CombinationWindow {
                                    phrase: rec_so_far,
                                    output_range: range,
                                    ends_in_prefix: true
                                });
                            } else if search_node.is_final() {
                                let final_output = output_so_far.cat(incr_output).cat(search_node.final_output());
                                // possibility number 3: we're at the end of our input, and not
                                // doing prefix matching, but that's okay because we've ended
                                // on a final node
                                out.push(CombinationWindow {
                                    phrase: rec_so_far,
                                    output_range: (final_output, final_output),
                                    ends_in_prefix: false
                                });
                            }
                        }
                    }
                },
                QueryWord::Prefix { key_range, .. } => {
                    if !ends_in_prefix {
                        return Err(PhraseSetError::new(
                            "The query submitted has a QueryWord::Prefix. This function only accepts QueryWord:Full"
                        ))
                    }
                    if let WordPrefixMatchResult::Found(state) = self.matches_prefix_range(
                        node.addr(),
                        output_so_far,
                        *key_range
                    ) {
                        // presumably the prefix is at the end, so we don't need to consider the
                        // possibility of recursing, just of being done; we can also assume AC is on
                        let mut rec_so_far = words_so_far.clone();
                        rec_so_far.push(word.clone());
                        let range = (PhraseSetMatchState::EndsInPrefix(state)).prefix_range(fst);
                        // possibility number 4: we're doing prefix matching, and we're at the end
                        // of our input and we're ending with a word range instead of a single word,
                        // so we've explored all the possible terminations that are reachable from
                        // this range and are pushing an output state that represents all of them
                        out.push(CombinationWindow {
                            phrase: rec_so_far,
                            output_range: range,
                            ends_in_prefix: true
                        });
                    }
                },
            }
        }
        Ok(())
    }

    /// Go from ID to set of word IDs, rather than the other way around. The approach is to start
    /// with the sought ID and subtract as we go, until we get to zero, at which point we wait
    /// until we hit a final state and then return. At each juncture, we're looking for the
    /// transition with the largest output that's still smaller than what we have left in our
    /// target ID
    pub fn get_by_id(&self, mut id: Output) -> Option<Vec<u32>> {
        let fst = &self.0;
        let mut node = fst.root();

        let mut word_id: Vec<u8> = Vec::with_capacity(4);
        word_id.push(0);

        let mut out: Vec<u32> = Vec::new();

        loop {
            let mut next_node: Option<_> = None;
            {
                let mut transitions = node.transitions().peekable();
                while let Some(current) = transitions.next() {
                    let found = match transitions.peek() {
                        // if the next one is too big, or we're on the last one, go with the
                        // current transition
                        Some(next) => next.out > id,
                        None => true,
                    };
                    if found {
                        if current.out > id {
                            return None;
                        }

                        id = id.sub(current.out);
                        word_id.push(current.inp);

                        if word_id.len() == 4 {
                            // this only fails if the slice isn't four bytes long, but it is ^
                            // so unwrap is safe
                            let word = (&word_id[..]).read_u32::<BigEndian>().unwrap();
                            out.push(word);
                            word_id.truncate(1);
                        }

                        let nn = fst.node(current.addr);
                        if id.value() == 0 && nn.is_final() {
                            return Some(out);
                        } else {
                            next_node = Some(nn);
                        }
                        break;
                    }
                }
            }

            match next_node {
                Some(n) => node = n,
                None => return None,
            }
        }
    }

    /// This function takes a given position in the graph, and checks to see if any words reachable
    /// from that position are within a range of word IDs that represent all words with a given
    /// prefix. This function is used within several different phrase graph exploration methods
    /// to do end-of-query checking in query modes where terminal partial words are allowed.
    ///
    /// The strategy is essentially: given a range, find the first word ID (three-byte sequence)
    /// that's greater than or equal to the lower bound of the range. If there is such an ID,
    /// and it's less than or equal to the upper bound, we've successfully found a match.
    ///
    /// If we have a match, we'll also want to find the maximum viable ID (the biggest one less
    /// than or equal to the upper bound of our range), in addition to the minimum viable ID, so
    /// that we can ascertain the minimum and maximum phrase IDs that are reachable from our
    /// current position given the constraints of our range.
    fn matches_prefix_range(&self, start_position: CompiledAddr, start_output: Output, key_range: (WordKey, WordKey)) -> WordPrefixMatchResult {
        let (sought_min_key, sought_max_key) = key_range;

		// self as fst
        let fst = &self.0;

        // get min value greater than or equal to the sought min
        let node0 = fst.node(start_position);
        let range0 = match self.find_first_gte(&node0, sought_min_key[0]) {
            Some(idx) => idx..node0.len(),
            None => { return WordPrefixMatchResult::NotFound; }
        };
        for i0 in range0 {
            let t0 = node0.transition(i0);

            let node1 = fst.node(t0.addr);
            // if our first found byte is greater than our first sought byte, we can just
            // use the first available byte from here, but otherwise we'll need to keep comparing.
            //
            // So like, if we'relooking for the first one >= [10, 6, 5], and we've found [11, _, _]
            // so far, [11, 0, _] is fine, but if we've found [10, _, _], the second byte will
            // still need to be at least 6 so that we end up >= [10, 6, 5]
            //
            // Note that we're also in a series of nested loops here, because what we first try
            // isn't guaranteed to work. We could find [10, 6, _], but maybe the highest last byte
            // is [10, 6, 2], and we'll ultimately still need to loop up to [11, _, _] to find
            // something we actually want
            let must_skip1 = t0.inp == sought_min_key[0];
            let range1 = if must_skip1 {
                match self.find_first_gte(&node1, sought_min_key[1]) {
                    Some(idx) => idx..node1.len(),
                    None => { continue; }
                }
            } else {
                0..node1.len()
            };
            for i1 in range1 {
                let t1 = node1.transition(i1);

                let node2 = fst.node(t1.addr);
                // similar to the above: are we still in the "greater than or equal" state,
                // or in the "min value is fine" state?
                let must_skip2 = must_skip1 && t1.inp == sought_min_key[1];
                let i2 = if node2.len() == 0 {
                    continue;
                } else if must_skip2 {
                    match self.find_first_gte(&node2, sought_min_key[2]) {
                        Some(idx) => idx,
                        None => { continue; }
                    }
                } else {
                    0
                };
                let t2 = node2.transition(i2);

                // we've got three bytes! woohoo!
                let next_after_min = [t0.inp, t1.inp, t2.inp];

                if next_after_min <= sought_max_key {
                    // we found the first triple after the minimum,
                    // but we also need the last before the maximum
                    //
                    // here we'll do the same as above: a three-byte walk, but mirror-imaged
                    // this should be guaranteed to succeed, since the next_after_min could also
                    // be a plausible last_before_max, so there's at least one valid one

                    let max_node0 = fst.node(start_position);
                    let max_range0 = match self.find_last_lte(&max_node0, sought_max_key[0]) {
                        Some(idx) => (0..(idx + 1)).rev(),
                        None => { return WordPrefixMatchResult::NotFound; }
                    };
                    for max_i0 in max_range0 {
                        let max_t0 = max_node0.transition(max_i0);

                        let max_node1 = fst.node(max_t0.addr);
                        let max_must_skip1 = max_t0.inp == sought_max_key[0];
                        let max_range1 = if max_must_skip1 {
                            match self.find_last_lte(&max_node1, sought_max_key[1]) {
                                Some(idx) => (0..(idx + 1)).rev(),
                                None => { continue; }
                            }
                        } else {
                            (0..max_node1.len()).rev()
                        };
                        for max_i1 in max_range1 {
                            let max_t1 = max_node1.transition(max_i1);

                            let max_node2 = fst.node(max_t1.addr);
                            let max_must_skip2 = max_must_skip1 && max_t1.inp == sought_max_key[1];
                            let max_i2 = if max_node2.len() == 0 {
                                continue;
                            } else if max_must_skip2 {
                                match self.find_last_lte(&max_node2, sought_max_key[2]) {
                                    Some(idx) => idx,
                                    None => { continue; }
                                }
                            } else {
                                max_node2.len() - 1
                            };
                            let max_t2 = max_node2.transition(max_i2);

                            // we've got three bytes! woohoo!
                            return WordPrefixMatchResult::Found(WordPrefixMatchState {
                                min_prefix_node: fst.node(t2.addr),
                                min_prefix_output: start_output.cat(t0.out).cat(t1.out).cat(t2.out),
                                max_prefix_node: fst.node(max_t2.addr),
                                max_prefix_output: start_output.cat(max_t0.out).cat(max_t1.out).cat(max_t2.out)
                            });
                        }
                    }

                    return WordPrefixMatchResult::Found(WordPrefixMatchState {
                        min_prefix_node: fst.node(t2.addr),
                        min_prefix_output: start_output.cat(t0.out).cat(t1.out).cat(t2.out),
                        max_prefix_node: fst.node(t2.addr),
                        max_prefix_output: start_output.cat(t0.out).cat(t1.out).cat(t2.out)
                    });
                } else {
                    return WordPrefixMatchResult::NotFound;
                }
            }
        }
        WordPrefixMatchResult::NotFound
    }

    // given a state in an FST, this finds the transition out with the smallest input that's at least
    // some minimum value, by binary search rather than by linear iteration
    #[inline(always)]
    fn find_first_gte(&self, node: &Node, inp: u8) -> Option<usize> {
        let len = node.len();
        match len {
            0 => None,
            1 => {
                let trans = node.transition(0);
                if trans.inp >= inp {
                    Some(0)
                } else {
                    None
                }
            },
            _ => {
                let mut low: usize = 0;
                let mut high: usize = len;
                let mut mid: usize;
                while low != high {
                    mid = (low + high) >> 1;
                    let trans = node.transition(mid);
                    if trans.inp < inp {
                        low = mid + 1;
                    } else {
                        high = mid;
                    }
                }
                if high < len {
                    Some(high)
                } else {
                    None
                }
            }
        }
    }

    // likewise, this is a binary search to find the transition with the largest input value that's
    // at most some maximum
    #[inline(always)]
    fn find_last_lte(&self, node: &Node, inp: u8) -> Option<usize> {
        let len = node.len() as isize;
        match len {
            0 => None,
            1 => {
                let trans = node.transition(0);
                if trans.inp <= inp {
                    Some(0)
                } else {
                    None
                }
            },
            _ => {
                let mut low: isize = -1;
                let mut high: isize = len - 1;
                let mut mid: isize;
                while low != high {
                    mid = ((low + high) >> 1) + 1;
                    let trans = node.transition(mid as usize);
                    if trans.inp <= inp {
                        low = mid;
                    } else {
                        high = mid - 1;
                    }
                }
                if high > -1 {
                    Some(high as usize)
                } else {
                    None
                }
            }
        }
    }

    pub fn as_fst(&self) -> &Fst {
        &self.0
    }

    pub fn get_max_id(&self) -> Output {
        // chase the maximum ID down the phrase tree
        let mut max_node: Node = self.0.root();
        let mut max_output: Output = Output::new(0);
        while max_node.len() != 0 {
            let t = max_node.transition(max_node.len() - 1);
            max_output = max_output.cat(t.out);
            max_node = self.0.node(t.addr);
        }
        max_output.cat(max_node.final_output())
    }

    /// Create from a raw byte sequence, which must be written by `PhraseSetBuilder`.
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, fst::Error> {
        Fst::from_bytes(bytes).map(PhraseSet)
    }

    #[cfg(feature = "mmap")]
    pub unsafe fn from_path<P: AsRef<Path>>(path: P) -> Result<Self, fst::Error> {
        Fst::from_path(path).map(PhraseSet)
    }

}

impl<'s, 'a> IntoStreamer<'a> for &'s PhraseSet {
    type Item = (&'a [u8], fst::raw::Output);
    type Into = fst::raw::Stream<'s>;

    fn into_stream(self) -> Self::Into {
        self.0.stream()
    }
}

#[derive(Debug)]
pub struct WordPrefixMatchState<'a> {
    min_prefix_node: Node<'a>,
    min_prefix_output: Output,
    max_prefix_node: Node<'a>,
    max_prefix_output: Output
}

enum WordPrefixMatchResult<'a> {
    NotFound,
    Found(WordPrefixMatchState<'a>)
}

#[derive(Debug)]
pub enum PhraseSetMatchState<'a> {
    EndsInFullWord {
        node: Node<'a>,
        output: Output
    },
    EndsInPrefix(WordPrefixMatchState<'a>)
}

impl<'a> PhraseSetMatchState<'a> {
    // retrieves the min and max IDs of all phrases that begin with the given prefix (which may
    // itself begin either with a word prefix or a full word)
    pub fn prefix_range(&self, fst: &'a Fst) -> (Output, Output) {
        let (min_node, min_output, max_node, max_output) = match self {
            PhraseSetMatchState::EndsInFullWord { node, output } => {
                (node, output, node, output)
            },
            PhraseSetMatchState::EndsInPrefix(state) => {
                (&state.min_prefix_node, &state.min_prefix_output, &state.max_prefix_node, &state.max_prefix_output)
            }
        };

        // for the minimum, whatever output state we've accumulated so far is the ID of the smallest
        // word reachable from where we are now
        let start = min_output.cat(min_node.final_output());

        // but whatever output we've accumulated on the max side is just the ID of the smallest
        // word reachable from our current max-side state; to find the largest ID, we need to
        // walk out to the edge of the graph, repeatedly choosing the highest outbound transition
        let mut max_node: Node = max_node.to_owned();
        let mut max_output: Output = max_output.to_owned();

        while max_node.len() != 0 {
            let t = max_node.transition(max_node.len() - 1);
            max_output = max_output.cat(t.out);
            max_node = fst.node(t.addr);
        }
        (start, max_output.cat(max_node.final_output()))
    }
}

pub enum PhraseSetLookupResult<'a> {
    NotFound,
    Found { fst: &'a Fst, match_state: PhraseSetMatchState<'a> }
}

impl<'a> PhraseSetLookupResult<'a> {
    /// Returns true if we've found any state (prefix or full word) given the input
    pub fn found(&self) -> bool {
        match self {
            PhraseSetLookupResult::NotFound => false,
            PhraseSetLookupResult::Found {..} => true
        }
    }

    /// Returns true if we've found a valid final state given the input
    pub fn found_final(&self) -> bool {
        match self {
            PhraseSetLookupResult::NotFound => false,
            PhraseSetLookupResult::Found { match_state, .. } => {
                match match_state {
                    PhraseSetMatchState::EndsInFullWord { node, .. } => node.is_final(),
                    PhraseSetMatchState::EndsInPrefix(..) => false
                }
            }
        }
    }

    /// Returns the ID (accumulated output) of the current state we've reached given the input
    pub fn id(&self) -> Option<Output> {
        match self {
            PhraseSetLookupResult::NotFound => None,
            PhraseSetLookupResult::Found { match_state, .. } => {
                match match_state {
                    PhraseSetMatchState::EndsInFullWord { node, output } => {
                        if node.is_final() {
                            Some(output.cat(node.final_output()))
                        } else {
                            None
                        }
                    },
                    PhraseSetMatchState::EndsInPrefix(..) => None
                }
            }
        }
    }

    /// Returns the range of output IDs reachable assuming the current state is a prefix
    pub fn range(&self) -> Option<(Output, Output)> {
        match self {
            PhraseSetLookupResult::NotFound => None,
            PhraseSetLookupResult::Found { fst, match_state } => Some(match_state.prefix_range(fst))
        }
    }

    /// Returns true if the current state is a valid prefix of other, longer phrases
    pub fn has_continuations(&self) -> bool {
        match self {
            PhraseSetLookupResult::NotFound => false,
            PhraseSetLookupResult::Found { match_state, .. } => {
                match match_state {
                    PhraseSetMatchState::EndsInFullWord { node, .. } => node.len() > 0,
                    PhraseSetMatchState::EndsInPrefix(state) => {
                        state.min_prefix_node.len() > 0 || (state.min_prefix_node.addr() != state.max_prefix_node.addr())
                    }
                }
            }
        }
    }
}

pub struct PhraseSetBuilder<W> {
    builder: Builder<W>,
    count: u64
}

impl PhraseSetBuilder<Vec<u8>> {
    pub fn memory() -> Self {
        PhraseSetBuilder { builder: Builder::memory(), count: 0 }
    }
}

impl<W: io::Write> PhraseSetBuilder<W> {
    pub fn new(wtr: W) -> Result<PhraseSetBuilder<W>, fst::Error> {
        Ok(PhraseSetBuilder { builder: Builder::new_type(wtr, 0)?, count: 0 })
    }

    /// Insert a phrase, specified as an array of word identifiers.
    pub fn insert(&mut self, phrase: &[u32]) -> Result<(), fst::Error> {
        let key = word_ids_to_key(phrase);
        self.builder.insert(key, self.count)?;
        self.count += 1;
        Ok(())
    }

    pub fn into_inner(self) -> Result<W, fst::Error> {
        self.builder.into_inner()
    }

    pub fn finish(self) -> Result<(), fst::Error> {
        self.builder.finish()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Combination {
    pub phrase: Vec<QueryWord>,
    pub output_range: (Output, Output)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CombinationWindow {
    pub phrase: Vec<QueryWord>,
    pub output_range: (Output, Output),
    pub ends_in_prefix: bool
}
