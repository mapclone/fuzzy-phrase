extern crate lazy_static;
extern crate strsim;
extern crate regex;
use std::fs::File;
use fst::Streamer;
use std::collections::BTreeMap;
use self::strsim::osa_distance;
use self::regex::Regex;
use super::*;
use self::query::QueryWord;
use self::util::three_byte_decode;

// the first chunk of tests assess the structure directly, with numerical inputs
#[test]
fn insert_phrases_memory() {
    let mut build = PhraseSetBuilder::memory();
    build.insert(&[1u32, 61_528_u32, 561_528u32]).unwrap();
    build.insert(&[61_528_u32, 561_528u32, 1u32]).unwrap();
    build.insert(&[561_528u32, 1u32, 61_528_u32]).unwrap();
    let bytes = build.into_inner().unwrap();

    let phrase_set = PhraseSet::from_bytes(bytes).unwrap();

    let mut keys = vec![];
    let mut stream = phrase_set.into_stream();
    while let Some(key) = stream.next() {
        keys.push(key.0.to_vec());
    }
    assert_eq!(
        keys,
        vec![
            vec![
                0u8, 0u8,   1u8,     // 1
                0u8, 240u8, 88u8,    // 61_528
                8u8, 145u8, 120u8    // 561_528
            ],
            vec![
                0u8, 240u8, 88u8,    // 61_528
                8u8, 145u8, 120u8,   // 561_528
                0u8, 0u8,   1u8      // 1
            ],
            vec![
                8u8, 145u8, 120u8,   // 561_528
                0u8, 0u8,   1u8,     // 1
                0u8, 240u8, 88u8     // 61_528
            ],
        ]
    );
}

#[test]
fn insert_phrases_file() {
    let wtr = io::BufWriter::new(File::create("/tmp/phrase-set.fst").unwrap());

    let mut build = PhraseSetBuilder::new(wtr).unwrap();
    build.insert(&[1u32, 61_528_u32, 561_528u32]).unwrap();
    build.insert(&[61_528_u32, 561_528u32, 1u32]).unwrap();
    build.insert(&[561_528u32, 1u32, 61_528_u32]).unwrap();
    build.finish().unwrap();

    let phrase_set = unsafe { PhraseSet::from_path("/tmp/phrase-set.fst") }.unwrap();

    let mut keys = vec![];
    let mut stream = phrase_set.into_stream();
    while let Some(key) = stream.next() {
        keys.push(key.0.to_vec());
    }
    assert_eq!(
        keys,
        vec![
            vec![
                0u8, 0u8,   1u8,     // 1
                0u8, 240u8, 88u8,    // 61_528
                8u8, 145u8, 120u8    // 561_528
            ],
            vec![
                0u8, 240u8, 88u8,    // 61_528
                8u8, 145u8, 120u8,   // 561_528
                0u8, 0u8,   1u8      // 1
            ],
            vec![
                8u8, 145u8, 120u8,   // 561_528
                0u8, 0u8,   1u8,     // 1
                0u8, 240u8, 88u8     // 61_528
            ],
        ]
    );
}

#[test]
fn contains_query() {
    let mut build = PhraseSetBuilder::memory();
    build.insert(&[1u32, 61_528_u32, 561_528u32]).unwrap();
    build.insert(&[61_528_u32, 561_528u32, 1u32]).unwrap();
    build.insert(&[561_528u32, 1u32, 61_528_u32]).unwrap();
    let bytes = build.into_inner().unwrap();

    let phrase_set = PhraseSet::from_bytes(bytes).unwrap();

    let words = vec![
        QueryWord::new_full(1u32, 0),
        QueryWord::new_full(61_528u32, 0),
        QueryWord::new_full(561_528u32, 0),
    ];

    let matching_phrase = [ words[0], words[1], words[2] ];
    let result = phrase_set.lookup(&matching_phrase);
    assert_eq!(true, result.found_final());
    assert_eq!(0, result.id().unwrap().value());

    let missing_phrase = [ words[0], words[1] ];
    let result = phrase_set.lookup(&missing_phrase);
    assert_eq!(false, result.found_final());
    assert_eq!(true, result.id().is_none());

    let prefix = QueryWord::new_prefix((561_528u32, 561_531u32));
    let has_prefix_phrase = [ words[0], words[1], prefix ];
    assert_eq!(false, phrase_set.lookup(&has_prefix_phrase).found_final());

    assert_eq!(&phrase_set.get_by_id(Output::new(2)).unwrap(), &[561_528u32, 1u32, 61_528_u32]);
    assert!(&phrase_set.get_by_id(Output::new(3)).is_none());
}

#[test]
fn contains_prefix_query() {
    let mut build = PhraseSetBuilder::memory();
    build.insert(&[1u32, 61_528_u32, 561_528u32]).unwrap();
    build.insert(&[61_528_u32, 561_528u32, 1u32]).unwrap();
    build.insert(&[561_528u32, 1u32, 61_528_u32]).unwrap();
    let bytes = build.into_inner().unwrap();

    let phrase_set = PhraseSet::from_bytes(bytes).unwrap();

    let words = vec![
        QueryWord::new_full(1u32, 0),
        QueryWord::new_full(61_528u32,  0),
        QueryWord::new_full(561_528u32, 0),
    ];

    let matching_phrase = [ words[0], words[1] ];
    let result = phrase_set.lookup(&matching_phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((0, 0), (range_start.value(), range_end.value()));

    let missing_phrase = [ words[0], words[2] ];
    assert_eq!(false, phrase_set.lookup(&missing_phrase).found());
}

#[test]
fn contains_prefix_range() {
    let mut build = PhraseSetBuilder::memory();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 1u8, 0u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 3u8, 2u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 3u8, 4u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 5u8, 6u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[4u8, 1u8, 1u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[4u8, 3u8, 3u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[4u8, 5u8, 5u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[6u8, 3u8, 4u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[6u8, 3u8, 7u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[6u8, 5u8, 8u8])]).unwrap();
    let bytes = build.into_inner().unwrap();
    let phrase_set = PhraseSet::from_bytes(bytes).unwrap();

    let words = vec![
        QueryWord::new_full(1u32,       0 ),
        QueryWord::new_full(61_528u32,  0 ),
        QueryWord::new_full(561_528u32, 0 ),
    ];

    // matches and the min edge of range
    let prefix_id_range = (
        three_byte_decode(&[6u8, 5u8, 8u8]),
        three_byte_decode(&[255u8, 255u8, 255u8]));
    let matching_prefix_min = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_prefix_min ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((9, 9), (range_start.value(), range_end.value()));

    // matches at the max edge of range
    let prefix_id_range = (
            three_byte_decode(&[0u8, 0u8, 0u8]),
            three_byte_decode(&[2u8, 1u8, 0u8]));
    let matching_prefix_max = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_prefix_max ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((0, 0), (range_start.value(), range_end.value()));

    // range is larger than possible outcomes
    let prefix_id_range = (
            three_byte_decode(&[2u8, 0u8, 255u8]),
            three_byte_decode(&[6u8, 5u8, 1u8]));
    let matching_prefix_larger = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_prefix_larger ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((0, 8), (range_start.value(), range_end.value()));

    // high side of range overlaps
    let prefix_id_range = (
            three_byte_decode(&[0u8, 0u8, 0u8]),
            three_byte_decode(&[2u8, 2u8, 1u8]));
    let matching_prefix_hi = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_prefix_hi ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((0, 0), (range_start.value(), range_end.value()));

    // low side of range overlaps
    let prefix_id_range = (
            three_byte_decode(&[6u8, 4u8, 1u8]),
            three_byte_decode(&[255u8, 255u8, 255u8]));
    let matching_prefix_low = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_prefix_low ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((9, 9), (range_start.value(), range_end.value()));

    // no overlap, too low
    let prefix_id_range = (
            three_byte_decode(&[0u8, 0u8, 0u8]),
            three_byte_decode(&[2u8, 0u8, 255u8]));
    let missing_prefix_low = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], missing_prefix_low ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(false, result.found());
    assert_eq!(true, result.range().is_none());

    // no overlap, too high
    let prefix_id_range = (
            three_byte_decode(&[6u8, 5u8, 9u8]),
            three_byte_decode(&[255u8, 255u8, 255u8]));
    let missing_prefix_hi = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], missing_prefix_hi ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(false, result.found());
    assert_eq!(true, result.range().is_none());

}

#[test]
fn contains_prefix_nested_range() {
    // in each of these cases, the sought range is within actual range, but the min and max
    // keys are not in the graph. that means we need to make sure that there is at least one
    // path that is actually within the sought range.
    let mut build = PhraseSetBuilder::memory();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 1u8, 0u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 3u8, 2u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 3u8, 4u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[2u8, 5u8, 6u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[4u8, 1u8, 1u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[4u8, 3u8, 3u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[4u8, 5u8, 5u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[6u8, 3u8, 4u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[6u8, 3u8, 7u8])]).unwrap();
    build.insert(&[1u32, 61_528_u32, three_byte_decode(&[6u8, 5u8, 8u8])]).unwrap();
    let bytes = build.into_inner().unwrap();
    let phrase_set = PhraseSet::from_bytes(bytes).unwrap();

    let words = vec![
        QueryWord::new_full(1u32,       0 ),
        QueryWord::new_full(61_528u32,  0 ),
        QueryWord::new_full(561_528u32, 0 ),
    ];

    // matches because (4, 3, 3) is in range
    let prefix_id_range = (
            three_byte_decode(&[4u8, 3u8, 1u8]),
            three_byte_decode(&[4u8, 3u8, 5u8]));
    let matching_two_bytes = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_two_bytes ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((5, 5), (range_start.value(), range_end.value()));

    // does not match because there is no actual path in sought range.
    let prefix_id_range = (
            three_byte_decode(&[4u8, 3u8, 0u8]),
            three_byte_decode(&[4u8, 3u8, 2u8]),
            ) ;
    let missing_two_bytes = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], missing_two_bytes ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(false, result.found());
    assert_eq!(true, result.range().is_none());

    // matches because (4, 1, 1) is in range
    let prefix_id_range = (
            three_byte_decode(&[4u8, 0u8, 1u8]),
            three_byte_decode(&[4u8, 2u8, 5u8]),
            ) ;
    let matching_one_byte = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_one_byte ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((4, 4), (range_start.value(), range_end.value()));

    // does not match because there is no actual path in sought range.
    let prefix_id_range = (
            three_byte_decode(&[4u8, 4u8, 0u8]),
            three_byte_decode(&[4u8, 5u8, 2u8]),
            ) ;
    let missing_one_byte = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], missing_one_byte ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(false, result.found());
    assert_eq!(true, result.range().is_none());

    // matches because (2, 5, 6) is in range. gives up searching high path because 0 is not in
    // the transitions for the byte after 4, which are [1, 3, 5].
    let prefix_id_range = (
            three_byte_decode(&[2u8, 4u8, 1u8]),
            three_byte_decode(&[4u8, 0u8, 0u8]),
            ) ;
    let matching_one_byte_lo = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_one_byte_lo ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((3, 3), (range_start.value(), range_end.value()));

    // misses because nothing is in range. gives up searching high path because 0 is not in
    // the transitions for the byte after 4, which are [1, 3, 5].
    let prefix_id_range = (
            three_byte_decode(&[2u8, 6u8, 1u8]),
            three_byte_decode(&[4u8, 0u8, 0u8]),
            ) ;
    let missing_one_byte_lo = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], missing_one_byte_lo ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(false, result.found());
    assert_eq!(true, result.range().is_none());

    // matches because (6, 3, 4) is in range. gives up searching low path because 7 is not in
    // the transitions for the byte after 4, which are [1, 3, 5].
    let prefix_id_range = (
            three_byte_decode(&[4u8, 7u8, 1u8]),
            three_byte_decode(&[6u8, 4u8, 0u8]),
            ) ;
    let matching_one_byte_hi = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_one_byte_hi ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((7, 8), (range_start.value(), range_end.value()));

    // misses because nothing is in range. gives up searching low path because 7 is not in
    // the transitions for the byte after 4, which are [1, 3, 5].
    let prefix_id_range = (
            three_byte_decode(&[4u8, 7u8, 1u8]),
            three_byte_decode(&[6u8, 2u8, 0u8]),
            ) ;
    let missing_one_byte_hi = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], missing_one_byte_hi ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(false, result.found());
    assert_eq!(true, result.range().is_none());

    // matches because (2, 1, 0) is on the low edge of the actual range, but sought range has
    // same min and max
    let prefix_id_range = (
            three_byte_decode(&[2u8, 1u8, 0u8]),
            three_byte_decode(&[2u8, 1u8, 0u8]),
            ) ;
    let matching_edge_low = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_edge_low ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((0, 0), (range_start.value(), range_end.value()));

    // matches because (2, 1, 0) is on the low edge of the actual range, but sought range has
    // same min and max
    let prefix_id_range = (
            three_byte_decode(&[6u8, 5u8, 8u8]),
            three_byte_decode(&[6u8, 5u8, 8u8]),
            ) ;
    let matching_edge_hi = QueryWord::new_prefix(prefix_id_range);
    let phrase = [ words[0], words[1], matching_edge_hi ];
    let result = phrase_set.lookup(&phrase);
    assert_eq!(true, result.found());
    let (range_start, range_end) = result.range().unwrap();
    assert_eq!((9, 9), (range_start.value(), range_end.value()));


}

// the next chunk of tests simulate practical use using sample data
lazy_static! {
    static ref PREFIX_DATA: &'static str = include_str!("../../benches/data/phrase_test_shared_prefix.txt");
    static ref TYPO_DATA: &'static str = include_str!("../../benches/data/phrase_test_typos.txt");
    static ref PHRASES: Vec<&'static str> = {
        // shared-prefix test set
        let mut phrases = PREFIX_DATA.trim().split("\n").collect::<Vec<&str>>();
        // typos test set
        phrases.extend(TYPO_DATA.trim().split("\n"));
        // take a few of the prefix test data set examples and add more phrases that are strict
        // prefixes of entries we already have to test windowed search
        phrases.extend(PREFIX_DATA.trim().split("\n").take(5).map(|phrase| {
            phrase.rsplitn(2, " ").skip(1).next().unwrap()
        }));
        phrases.sort();
        phrases
    };
    static ref WORDS: BTreeMap<&'static str, u32> = {
        let mut words: BTreeMap<&'static str, u32> = BTreeMap::new();
        for phrase in PHRASES.iter() {
            for word in phrase.split(' ') {
                words.insert(word, 0);
            }
        }
        let mut id: u32 = 0;
        for (_key, value) in words.iter_mut() {
            *value = id;
            // space the IDs out some
            id += 1000;
        }
        words
    };
    static ref DISTANCES: BTreeMap<u32, Vec<(u32, u8)>> = {
        let mut out: BTreeMap<u32, Vec<(u32, u8)>> = BTreeMap::new();

        let mut non_number: Vec<(&'static str, u32)> = Vec::new();
        let number_chars = Regex::new("[0-9#]").unwrap();
        for (word, id) in WORDS.iter() {
            out.insert(*id, vec![(*id, 0)]);
            if !number_chars.is_match(word) {
                non_number.push((*word, *id));
            }
        }

        for (word1, id1) in &non_number {
            for (word2, id2) in &non_number {
                if osa_distance(word1, word2) == 1 {
                    out.get_mut(id1).unwrap().push((*id2, 1));
                }
            }
        }

        out
    };
    static ref ID_PHRASES: Vec<Vec<u32>> = {
        let mut id_phrases = PHRASES.iter().map(|phrase| {
            phrase.split(' ').map(|w| WORDS[w]).collect::<Vec<_>>()
        }).collect::<Vec<_>>();
        id_phrases.sort();
        id_phrases.dedup();
        id_phrases
    };
    static ref SET: PhraseSet = {
        let mut builder = PhraseSetBuilder::memory();

        for id_phrase in ID_PHRASES.iter() {
            builder.insert(&id_phrase).unwrap();
        }
        let bytes = builder.into_inner().unwrap();
        PhraseSet::from_bytes(bytes).unwrap()
    };
}

fn get_full(phrase: &str) -> Vec<QueryWord> {
    phrase.split(' ').map(
        |w| QueryWord::new_full(WORDS[w], 0)
    ).collect::<Vec<_>>()
}

fn get_prefix(phrase: &str) -> Vec<QueryWord> {
    let words: Vec<&str> = phrase.split(' ').collect();
    let mut out = words[..(words.len() - 1)].iter().map(
        |w| QueryWord::new_full(*WORDS.get(w).unwrap(), 0)
    ).collect::<Vec<QueryWord>>();
    let last = &words[words.len() - 1];
    let prefix_match = WORDS.iter().filter(|(k, _v)| k.starts_with(last)).collect::<Vec<_>>();
    out.push(QueryWord::new_prefix((*prefix_match[0].1, *prefix_match.last().unwrap().1)));
    out
}

fn get_expected_range(encoded_phrase: &[QueryWord], exact_match: bool) -> Option<(u64, u64)> {
    let matches = |id_phrase: &Vec<u32>| {
        if
            (exact_match && id_phrase.len() != encoded_phrase.len()) ||
            (id_phrase.len() < encoded_phrase.len())
        {
            false
        } else {
            for (i, qw) in encoded_phrase.iter().enumerate() {
                let matches = match qw {
                    QueryWord::Full { id, .. } => *id == id_phrase[i],
                    QueryWord::Prefix { id_range: (start, end), .. } => *start <= id_phrase[i] && *end >= id_phrase[i]
                };
                if !matches {
                    return false;
                }
            }
            true
        }
    };

    let seek = ID_PHRASES.iter().position(matches);
    match seek {
        Some(start) => {
            let len = ID_PHRASES[start..].iter().take_while(|m| matches(*m)).count();
            Some((start as u64, (start + len - 1) as u64))
        },
        None => None
    }
}

fn get_full_combination(phrase: &str) -> Combination {
    let qw_phrase = get_full(phrase);
    let range = get_expected_range(&qw_phrase, false).unwrap();
    Combination {
        phrase: qw_phrase,
        output_range: (Output::new(range.0), Output::new(range.1))
    }
}

fn get_prefix_combination(phrase: &str) -> Combination {
    let qw_phrase = get_prefix(phrase);
    let range = get_expected_range(&qw_phrase, false).unwrap();
    Combination {
        phrase: qw_phrase,
        output_range: (Output::new(range.0), Output::new(range.1))
    }
}

fn get_full_window(phrase: &str, ends_in_prefix: bool) -> CombinationWindow {
    let qw_phrase = get_full(phrase);
    let range = get_expected_range(&qw_phrase, !ends_in_prefix).unwrap();
    CombinationWindow {
        phrase: qw_phrase,
        output_range: (Output::new(range.0), Output::new(range.1)),
        ends_in_prefix
    }
}

fn get_prefix_window(phrase: &str, ends_in_prefix: bool) -> CombinationWindow {
    let combination = get_prefix_combination(phrase);
    CombinationWindow {
        phrase: combination.phrase,
        output_range: combination.output_range,
        ends_in_prefix
    }
}

#[test]
fn sample_contains() {
    // just test everything
    for phrase in PHRASES.iter() {
        assert!(SET.lookup(
            &get_full(phrase)
        ).found_final());
    }
}

#[test]
fn sample_doesnt_contain() {
    // construct some artificial broken examples by reversing the sequence of good ones
    for phrase in PHRASES.iter() {
        let mut inverse = get_full(phrase);
        inverse.reverse();
        assert!(!SET.lookup(
            &inverse
        ).found_final());
    }

    // a couple manual ones
    let contains = |phrase| {
        SET.lookup(&get_full(phrase)).found_final()
    };

    // typo
    assert!(!contains("15## Hillis Market Rd"));
    // prefix
    assert!(!contains("40# Ivy"));
}

#[test]
fn sample_contains_prefix() {
    // being exhaustive is a little laborious, so just try a bunch of specific ones
    let contains_prefix = |phrase| {
        SET.lookup(&get_prefix(phrase)).found()
    };

    assert!(contains_prefix("8"));
    assert!(contains_prefix("84"));
    assert!(contains_prefix("84#"));
    assert!(contains_prefix("84# "));
    assert!(contains_prefix("84# G"));
    assert!(contains_prefix("84# Suchava Dr"));

    assert!(!contains_prefix("84# Suchava Dr Ln"));
    assert!(!contains_prefix("Suchava Dr"));
    // note that we don't test any that include words we don't know about -- in the broader
    // scheme, that's not our job
}

#[test]
fn sample_check_prefix_ranges() {
    let get_range = |phrase| {
        SET.lookup(&get_prefix(phrase)).range().map(|(a, b)| (a.value(), b.value()))
    };

    assert_eq!(get_range("8"), get_expected_range(&get_prefix("8"), false));
    assert_eq!(get_range("84"), get_expected_range(&get_prefix("84"), false));
    assert_eq!(get_range("84#"), get_expected_range(&get_prefix("84#"), false));
    assert_eq!(get_range("84# "), get_expected_range(&get_prefix("84# "), false));
    assert_eq!(get_range("84# G"), get_expected_range(&get_prefix("84# G"), false));
    assert_eq!(get_range("84# Suchava Dr"), get_expected_range(&get_prefix("84# Suchava Dr"), false));
}

#[test]
fn sample_check_continuations() {
    assert_eq!(true, SET.lookup(&get_prefix("8")).has_continuations());
    assert_eq!(true, SET.lookup(&get_prefix("84# Gleason Hollow")).has_continuations());
    assert_eq!(false, SET.lookup(&get_prefix("84# Suchava Dr")).has_continuations());
    assert_eq!(false, SET.lookup(&get_prefix("84# Gleason Suchava")).has_continuations());
}

fn get_full_variants(phrase: &str) -> Vec<Vec<QueryWord>> {
    phrase.split(' ').map(
        |w| DISTANCES[&WORDS[w]].iter().map(
            |(id, distance)| QueryWord::new_full(*id, *distance)
        ).collect::<Vec<_>>()
    ).collect::<Vec<_>>()
}

fn get_prefix_variants(phrase: &str) -> Vec<Vec<QueryWord>> {
    let words: Vec<&str> = phrase.split(' ').collect();
    let mut out = words[..(words.len() - 1)].iter().map(
        |w| DISTANCES.get(WORDS.get(w).unwrap()).unwrap().iter().map(
            |(id, distance)| QueryWord::new_full(*id, *distance)
        ).collect::<Vec<_>>()
    ).collect::<Vec<Vec<QueryWord>>>();

    let last = &words[words.len() - 1];
    let prefix_match = WORDS.iter().filter(|(k, _v)| k.starts_with(last)).collect::<Vec<_>>();
    let mut last_group = vec![QueryWord::new_prefix((*prefix_match[0].1, *prefix_match.last().unwrap().1))];
    if let Some(id) = WORDS.get(last) {
        for (id, distance) in DISTANCES.get(id).unwrap() {
            if *distance == 1u8 {
                last_group.push(QueryWord::new_full(*id, *distance));
            }
        }
    }
    out.push(last_group);

    out
}

#[test]
fn sample_match_combinations() {
    let correct = get_full_combination("53# Country View Dr");
    let no_typo = SET.match_combinations(&get_full_variants("53# Country View Dr"), 1).unwrap();
    assert!(no_typo == vec![correct.clone()]);

    let typo = SET.match_combinations(&get_full_variants("53# County View Dr"), 1).unwrap();
    assert!(typo != vec![correct.clone()]);
}

#[test]
fn sample_match_combinations_as_prefixes() {
    let correct1 = get_prefix_combination("53# Country");
    let no_typo1 = SET.match_combinations_as_prefixes(&get_prefix_variants("53# Country"), 1).unwrap();
    assert!(no_typo1 == vec![correct1.clone()]);

    let typo1 = SET.match_combinations_as_prefixes(&get_prefix_variants("53# County"), 1).unwrap();
    assert!(typo1 != vec![correct1.clone()]);

    let correct2 = get_prefix_combination("53# Country V");
    let no_typo2 = SET.match_combinations_as_prefixes(&get_prefix_variants("53# Country V"), 1).unwrap();
    assert!(no_typo2 == vec![correct2.clone()]);

    let typo2 = SET.match_combinations_as_prefixes(&get_prefix_variants("53# County V"), 1).unwrap();
    assert!(typo2 != vec![correct2.clone()]);
}

#[test]
fn sample_contains_windows_simple() {
    // just test everything
    let max_phrase_dist = 2;
    let ends_in_prefix = false;
    for phrase in PHRASES.iter() {
        let window = get_full_window(phrase, false);
        let word_possibilities = get_full_variants(phrase);
        let results = SET.match_combinations_as_windows(
            &word_possibilities,
            max_phrase_dist,
            ends_in_prefix
        ).unwrap();
        assert!(results.len() > 0);
        assert!(results.iter().any(|r| r == &window));
    }
}

#[test]
fn sample_match_combinations_as_windows_all_full() {
    // just test everything
    let max_phrase_dist = 2;
    for phrase in PHRASES.iter() {
        let mut query_phrase = get_full(phrase);
        let mut word_possibilities = get_full_variants(phrase);

        // trim the last element to test prefix functionality
        query_phrase.pop();
        word_possibilities.pop();

        let range = get_expected_range(&query_phrase, false).unwrap();
        let window = CombinationWindow {
            phrase: query_phrase,
            output_range: (Output::new(range.0), Output::new(range.1)),
            ends_in_prefix: true
        };

        let results = SET.match_combinations_as_windows(
            &word_possibilities,
            max_phrase_dist,
            true
        ).unwrap();

        assert!(results.len() > 0);
        assert!(results.iter().any(|r| r == &window));
    }
}

#[test]
fn sample_match_combinations_as_windows_all_prefix() {
    // just test everything
    let max_phrase_dist = 2;
    for phrase in PHRASES.iter() {
        let window = get_prefix_window(phrase, true);
        let word_possibilities = get_prefix_variants(phrase);

        let results = SET.match_combinations_as_windows(
            &word_possibilities,
            max_phrase_dist,
            true
        ).unwrap();
        assert!(results.len() > 0);
        assert!(results.iter().any(|r| r == &window));
    }
}

#[test]
fn sample_prefix_contains_windows_overlap() {
    let word_possibilities = get_prefix_variants("84# Gleason Hollow Rd");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        true
    ).unwrap();

    // this should match two different results, one of which is a prefix of the other, since we
    // augmented the data with some prefix examples
    assert_eq!(
        results,
        vec![
            // this one doesn't end in the prefix
            get_full_window("84# Gleason Hollow", false),
            // but this one does
            get_prefix_window("84# Gleason Hollow Rd", true),
        ]
    );
}

#[test]
fn sample_prefix_contains_windows_substring() {
    // this works
    let word_possibilities = get_prefix_variants("59 Old Ne");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        true
    ).unwrap();
    assert_eq!(results, vec![get_prefix_window("59 Old Ne", true)]);

    // but this will fail because we can't window-recurse with eip=false and a PrefixWord
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        false
    );
    assert!(results.is_err());

    // so let's try with just full words...
    // except this also doesn't work when searched non-prefix
    let word_possibilities = get_full_variants("59 Old New");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        false
    ).unwrap();
    assert_eq!(results, vec![]);

    // and it doesn't work with crap added to the end of it
    let word_possibilities = get_prefix_variants("59 Old New Gleason");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        true
    ).unwrap();
    assert_eq!(results, vec![]);

    // or to the beginning -- we'd need to have a different start position
    let word_possibilities = get_prefix_variants("Gleason 59 Old New");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        true
    ).unwrap();
    assert_eq!(results, vec![]);

    // on the other hand, we *should* be able to find a whole string with other stuff after it
    let word_possibilities = get_prefix_variants("59 Old New Milford Rd Gleason");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        true
    ).unwrap();
    assert_eq!(results, vec![get_full_window("59 Old New Milford Rd", false)]);

    // and should also work with no prefixes
    let word_possibilities = get_full_variants("59 Old New Milford Rd Gleason");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        false
    ).unwrap();
    assert_eq!(results, vec![get_full_window("59 Old New Milford Rd", false)]);

    // on the other hand, it still shouldn't work with stuff at the beginning
    let word_possibilities = get_prefix_variants("Gleason 59 Old New Milford Rd");
    let results = SET.match_combinations_as_windows(
        &word_possibilities,
        1,
        true
    ).unwrap();
    assert_eq!(results, vec![]);
}

#[test]
fn sample_get_by_id() {
    for (id, phrase) in ID_PHRASES.iter().enumerate() {
        assert_eq!(&SET.get_by_id(Output::new(id as u64)).unwrap(), phrase);
    }
}