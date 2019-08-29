use itertools::Itertools;
use fst::raw::{Fst, Node, Output};

use ::phrase::util::three_byte_decode;

#[derive(Debug, Clone)]
pub struct PrefixBin {
    pub prefix: String,
    pub first: Output,
    pub last: Output,
    pub size: usize,
}

#[derive(Debug, Clone)]
pub struct BinGroup<'a> {
    pub prefix_bin: PrefixBin,
    words: Vec<PrefixWord<'a>>
}

#[derive(Debug, Clone)]
struct PrefixWord<'a> {
    node: Node<'a>,
    first: Output,
    last: Output,
    size: usize,
    word_id: u32,
}

pub fn subdivide_word<'a>(fst: &'a Fst, word_root: &Node, id_base: Output, max_id_for_word: Output, max_bin_size: usize, word_list: &[String]) -> Vec<BinGroup<'a>> {
    let mut naive_bins: Vec<BinGroup> = Vec::new();
    let mut current_words: Vec<PrefixWord> = Vec::new();
    let mut current_prefix: String = "".to_string();

    if word_root.is_final() {
        naive_bins.push(BinGroup {
            prefix_bin: PrefixBin {
                prefix: "".to_string(),
                first: id_base,
                last: id_base,
                size: 1
            },
            words: Vec::new()
        });
    }

    let mut transitions0 = word_root.transitions().peekable();
    while let Some(t0) = transitions0.next() {
        let min_output0 = id_base.cat(t0.out);
        let max_output0 = match transitions0.peek() {
            Some(next) => Output::new(id_base.cat(next.out).value() - 1),
            None => max_id_for_word
        };
        let node1 = fst.node(t0.addr);
        let mut transitions1 = node1.transitions().peekable();

        while let Some(t1) = transitions1.next() {
            let min_output1 = min_output0.cat(t1.out);
            let max_output1 = match transitions1.peek() {
                Some(next) => Output::new(min_output0.cat(next.out).value() - 1),
                None => max_output0
            };
            let node2 = fst.node(t1.addr);
            let mut transitions2 = node2.transitions().peekable();

            while let Some(t2) = transitions2.next() {
                let min_output2 = min_output1.cat(t2.out);
                let max_output2 = match transitions2.peek() {
                    Some(next) => Output::new(min_output1.cat(next.out).value() - 1),
                    None => max_output1
                };
                let word_id = three_byte_decode(&[t0.inp, t1.inp, t2.inp]);
                let word = &word_list[word_id as usize];
                let prefix: String = word.chars().take(1).collect();
                if prefix != current_prefix {
                    if current_words.len() > 0 {
                        let mut old_current_words = Vec::new();
                        std::mem::swap(&mut current_words, &mut old_current_words);
                        naive_bins.push(words_to_bin(current_prefix.clone(), old_current_words));
                    }
                    current_prefix = prefix;
                }
                current_words.push(PrefixWord {
                    node: fst.node(t2.addr),
                    first: min_output2,
                    last: max_output2,
                    size: (max_output2.value() - min_output2.value() + 1) as usize,
                    word_id: word_id
                });
            }
        }
    }
    if current_words.len() > 0 {
        naive_bins.push(words_to_bin(current_prefix, current_words));
    }

    let mut out = Vec::new();
    for bin in naive_bins.into_iter() {
        if bin.prefix_bin.size > max_bin_size {
            let subdivided = subdivide_bin(fst, &bin, 2, max_bin_size, word_list);
            out.extend_from_slice(&subdivided);
        } else {
            out.push(bin);
        }
    }
    out
}

fn subdivide_bin<'a>(fst: &'a Fst, bin: &BinGroup<'a>, depth: usize, max_bin_size: usize, word_list: &[String]) -> Vec<BinGroup<'a>> {
    let rebinned = bin.words.iter().group_by(|w| word_list[w.word_id as usize].chars().take(depth).collect::<String>());
    let mut out = Vec::new();
    for (prefix, group) in rebinned.into_iter() {
        let inner_bin = words_to_bin(prefix, group.cloned().collect());
        if inner_bin.prefix_bin.size > max_bin_size {
            if inner_bin.words.len() > 1 {
                let subdivided = subdivide_bin(fst, &inner_bin, depth + 1, max_bin_size, word_list);
                out.extend_from_slice(&subdivided);
            } else {
                let word = &inner_bin.words[0];
                let subdivided = subdivide_word(fst, &word.node, word.first, word.last, max_bin_size, word_list);
                let new_prefix = inner_bin.prefix_bin.prefix.rsplit(" ").nth(1).unwrap_or_else(|| "").to_string() + " " + &word_list[word.word_id as usize];
                for mut sub_bin in subdivided {
                    // our current inner_bin ends in a partial word that we're nuking, so pop that
                    sub_bin.prefix_bin.prefix = ("".to_owned() + &new_prefix + " " + &sub_bin.prefix_bin.prefix).trim().to_string();
                    out.push(sub_bin);
                }
            }
        } else {
            out.push(inner_bin);
        }
    }
    out
}

fn words_to_bin(prefix: String, words: Vec<PrefixWord>) -> BinGroup {
    let first = words[0].first;
    let last = words.last().unwrap().last;
    let size = (last.value() - first.value() + 1) as usize;
    BinGroup { prefix_bin: PrefixBin { prefix, first, last, size }, words }
}