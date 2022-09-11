#[cfg(test)] extern crate tempfile;
#[cfg(test)] extern crate rand;
#[cfg(test)] extern crate lazy_static;
#[cfg(test)] extern crate test_utils;

use super::*;
use glue::fuzz_tests::test_utils::*;
use glue::fuzz_tests::rand::Rng;
use std::io::Read;
use std::fs;
use itertools;

lazy_static! {
    static ref DIR: tempfile::TempDir = tempfile::tempdir().unwrap();
    static ref DATA: String = {
        let test_data = ensure_data("phrase", "us", "en", "latn", true);

        let mut file = fs::File::open(test_data).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        data
    };
    static ref PHRASES: Vec<&'static str> = {
        DATA.trim().split("\n").collect::<Vec<&str>>()
    };
    static ref TMP_TO_FINAL: Vec<u32> = {
        let mut builder = FuzzyPhraseSetBuilder::new(&DIR.path()).unwrap();
        for (i, phrase) in PHRASES.iter().enumerate() {
            let tmp_id = builder.insert_str(phrase).unwrap();
            // later we're going to assume that the tmp_id a phrase was assigned is the same
            // as its index in PHRASES, so assert that this is true now
            assert_eq!(tmp_id, i as u32);
        }
        builder.finish().unwrap()
    };
    static ref SET: FuzzyPhraseSet = {
        lazy_static::initialize(&TMP_TO_FINAL);
        FuzzyPhraseSet::from_path(&DIR.path()).unwrap()
    };
}

#[test]
#[ignore]
fn glue_fuzztest_build() {
    lazy_static::initialize(&SET);
}

#[test]
#[ignore]
fn glue_fuzztest_match() {
    let mut rng = rand::thread_rng();
    for _i in 0..500 {
        // per previous, this is both the position of a phrase in PHRASES and the tmp_id it was
        // assigned when it was inserted, so we can use it to map to the final ID later
        let phrase_idx = rng.gen_range(0, PHRASES.len());
        let phrase = PHRASES[phrase_idx];
        let damaged = get_damaged_phrase(phrase, |w| SET.can_fuzzy_match(w) && w.chars().count() > 2);
        let results = SET.fuzzy_match_str(&damaged.as_str(), 1, 1, EndingType::NonPrefix);

        assert!(results.is_ok());
        if let Ok(res) = results {
            let matching: Vec<_> = res.iter().filter(
                |result| itertools::join(&result.phrase, " ").as_str() == phrase
            ).collect();
            assert!(matching.len() > 0);
            let expected_id = TMP_TO_FINAL[phrase_idx];
            assert!(matching.iter().any(
                |result| result.phrase_id_range == (expected_id, expected_id)
            ));
        }
    }
}

#[test]
#[ignore]
fn glue_fuzztest_match_prefix() {
    let mut rng = rand::thread_rng();
    for _i in 0..500 {
        let phrase_idx = rng.gen_range(0, PHRASES.len());
        let phrase = PHRASES[phrase_idx];
        let damaged = get_damaged_prefix(phrase, |w| SET.can_fuzzy_match(w) && w.chars().count() > 2);
        let results = SET.fuzzy_match_str(&damaged.as_str(), 1, 1, EndingType::AnyPrefix);

        assert!(results.is_ok());
        if let Ok(res) = results {
            let prefix_match_results: Vec<_> = res.iter()
                .filter(|result| phrase.starts_with(itertools::join(&result.phrase, " ").as_str()))
                .collect();

            assert!(prefix_match_results.len() > 0);

            let expected_id = TMP_TO_FINAL[phrase_idx];
            assert!(prefix_match_results.iter().any(|result|
                expected_id >= result.phrase_id_range.0 && expected_id <= result.phrase_id_range.1
            ));
        }

        let wb_results = SET.fuzzy_match_str(&damaged.as_str(), 1, 1, EndingType::WordBoundaryPrefix);
        assert!(wb_results.is_ok());
        if let Ok(res) = wb_results {
            let prefix_match_results: Vec<_> = res.iter()
                .filter(|result| phrase.starts_with(itertools::join(&result.phrase, " ").as_str()))
                .collect();
            // if the last word is complete, we should definitely match as above; if not, maybe maybe not
            let words: Vec<_> = damaged.split(' ').collect();
            let last_word = words.last().unwrap();
            if &phrase.split(' ').nth(words.len() - 1).unwrap() == last_word {
                assert!(prefix_match_results.len() > 0);
            } else {
                assert!(prefix_match_results.iter().all(|result| result.phrase.last().unwrap() == last_word || result.edit_distance > 0));
            }
        }
    }
}

#[test]
#[ignore]
fn glue_fuzztest_windowed_multi_equivalent() {
    let cities: Vec<&str> = include_str!("../../benches/data/phrase_test_cities.txt").trim().split("\n").collect();
    let states: Vec<&str> = include_str!("../../benches/data/phrase_test_states.txt").trim().split("\n").collect();
    let mut rng = rand::thread_rng();
    let mut augmented_phrases: Vec<String> = Vec::with_capacity(1000);
    for _i in 0..1000 {
        let phrase = rng.choose(&PHRASES).unwrap();
        let damaged = get_damaged_phrase(phrase, |w| SET.can_fuzzy_match(w) && w.chars().count() > 2);
        let zip: u32 = rng.gen_range(10000, 99999);

        // make a string with the components in random order
        let mut augmented_vec = vec![
            damaged,
            rng.choose(&cities).unwrap().to_string(),
            rng.choose(&states).unwrap().to_string(),
            zip.to_string()
        ];
        rng.shuffle(augmented_vec.as_mut_slice());
        let augmented = augmented_vec.join(" ");
        augmented_phrases.push(augmented);
    }

    for phrase in augmented_phrases.iter() {
        let tokens: Vec<_> = phrase.split(" ").collect();
        let mut variants: Vec<(Vec<&str>, EndingType)> = Vec::new();
        let mut variant_starts: Vec<usize> = Vec::new();
        for start in 0..tokens.len() {
            for end in start..tokens.len() {
                variants.push((tokens[start..(end + 1)].to_vec(), EndingType::NonPrefix));
                variant_starts.push(start);
            }
        }
        let individual_match_result = variants.iter().map(|v| SET.fuzzy_match(v.0.as_slice(), 1, 1, EndingType::NonPrefix).unwrap()).collect::<Vec<_>>();
        let multi_match_result = SET.fuzzy_match_multi(variants.as_slice(), 1, 1).unwrap();

        // check if the multi match results and the one-by-one match results are identical
        assert_eq!(individual_match_result, multi_match_result);

        // to make sure the windowed match and multi windowed match give the same results, we need
        // to reformat the multi-match results to look like windowed match results based on the
        // start position and length of each variant
        let mut windowed_match_result = SET.fuzzy_match_windows(tokens.as_slice(), 1, 1, EndingType::NonPrefix).unwrap();
        let mut emulated_windowed_match_result: Vec<FuzzyWindowResult> = Vec::new();
        for i in 0..multi_match_result.len() {
            for result in &multi_match_result[i] {
                emulated_windowed_match_result.push(FuzzyWindowResult {
                    phrase: result.phrase.clone(),
                    edit_distance: result.edit_distance,
                    start_position: variant_starts[i],
                    ending_type: EndingType::NonPrefix,
                    phrase_id_range: result.phrase_id_range
                });
            }
        }

        windowed_match_result.sort();
        emulated_windowed_match_result.sort();

        assert_eq!(windowed_match_result, emulated_windowed_match_result);
    }
}

#[test]
#[ignore]
fn glue_fuzztest_windowed_multi_equivalent_prefix() {
    let cities: Vec<&str> = include_str!("../../benches/data/phrase_test_cities.txt").trim().split("\n").collect();
    let states: Vec<&str> = include_str!("../../benches/data/phrase_test_states.txt").trim().split("\n").collect();
    let mut rng = rand::thread_rng();
    let mut augmented_phrases: Vec<String> = Vec::with_capacity(1000);
    for _i in 0..100 {
        let phrase = rng.choose(&PHRASES).unwrap();
        let damaged = get_damaged_phrase(phrase, |w| SET.can_fuzzy_match(w) && w.chars().count() > 2);
        let zip: u32 = rng.gen_range(10000, 99999);

        // make a string with the components in random order
        let mut augmented_vec = vec![
            damaged,
            rng.choose(&cities).unwrap().to_string(),
            rng.choose(&states).unwrap().to_string(),
            zip.to_string()
        ];
        rng.shuffle(augmented_vec.as_mut_slice());
        let augmented = augmented_vec.join(" ");
        augmented_phrases.push(random_trunc(&augmented));
    }

    for phrase in augmented_phrases.iter() {
        let tokens: Vec<_> = phrase.split(" ").collect();
        let mut variants: Vec<(Vec<&str>, EndingType)> = Vec::new();
        let mut variant_starts: Vec<usize> = Vec::new();
        for start in 0..tokens.len() {
            for end in start..tokens.len() {
                let ending_type = if end + 1 == tokens.len() {
                    EndingType::AnyPrefix
                } else {
                    EndingType::NonPrefix
                };
                variants.push((tokens[start..(end + 1)].to_vec(), ending_type));
                variant_starts.push(start);
            }
        }
        let individual_match_result = variants.iter().map(
            |v| SET.fuzzy_match(v.0.as_slice(), 1, 1, v.1).unwrap()
        ).collect::<Vec<_>>();
        let multi_match_result = SET.fuzzy_match_multi(variants.as_slice(), 1, 1).unwrap();

        // check if the multi match results and the one-by-one match results are identical
        assert_eq!(individual_match_result, multi_match_result);

        // to make sure the windowed match and multi windowed match give the same results, we need
        // to reformat the multi-match results to look like windowed match results based on the
        // start position and length of each variant
        let mut windowed_match_result = SET.fuzzy_match_windows(tokens.as_slice(), 1, 1, EndingType::AnyPrefix).unwrap();
        let mut emulated_windowed_match_result: Vec<FuzzyWindowResult> = Vec::new();
        for i in 0..multi_match_result.len() {
            for result in &multi_match_result[i] {
                emulated_windowed_match_result.push(FuzzyWindowResult {
                    phrase: result.phrase.clone(),
                    edit_distance: result.edit_distance,
                    start_position: variant_starts[i],
                    ending_type: result.ending_type,
                    phrase_id_range: result.phrase_id_range
                });
            }
        }

        windowed_match_result.sort();
        emulated_windowed_match_result.sort();

        assert_eq!(windowed_match_result, emulated_windowed_match_result);
    }
}