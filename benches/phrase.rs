use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::{BTreeSet, BTreeMap};
use std::rc::Rc;
use std::env;
use rand::{thread_rng, Rng};
use criterion::{Criterion, Fun, Bencher};
use fuzzy_phrase::{PhraseSet, PhraseSetBuilder};
use fuzzy_phrase::phrase::query::{QueryWord, QueryPhrase};

fn tokenize(s: &str) -> Vec<String> {
    s.split(" ").map(|w| w.to_lowercase()).collect()
}

pub fn build_phrase_graph(file_loc: &str) -> (BTreeMap<String, u32>, PhraseSet) {
    // fetch data and build the structures
    let mut autoinc = 0;
    let f = File::open(file_loc).expect("tried to open_file");
    let file_buf = BufReader::new(&f);

    // Build a vocabulary of the unique words in the test set
    let mut vocabulary = BTreeSet::new();
    for line in file_buf.lines() {
       let s: String = line.unwrap();
       let words = tokenize(s.as_str());
       for word in words {
           vocabulary.insert(word);
       }
    }

    // Build a map from words to ids
    let mut word_to_id: BTreeMap<String, u32> = BTreeMap::new();
    for word in &vocabulary {
        word_to_id.insert(word.to_string(), autoinc);
        autoinc += 1;
    }

    let f = File::open(file_loc).expect("tried to open_file");
    let file_buf = BufReader::new(&f);
    let mut phrases: Vec<Vec<u32>> = vec![];
    for line in file_buf.lines() {
       let s: String = line.unwrap();
       let mut word_ids: Vec<u32> = vec![];
       let words = tokenize(s.as_str());
       for word in words {
           let word_id = word_to_id.get(&word).unwrap();
           word_ids.push(*word_id);
       }
       phrases.push(word_ids);
    }

    phrases.sort();

    let mut build = PhraseSetBuilder::memory();

    for phrase in phrases.iter() {
        build.insert(&phrase).unwrap();
    }

    let bytes = build.into_inner().unwrap();

    let phrase_set = PhraseSet::from_bytes(bytes).unwrap();
    return (word_to_id, phrase_set)
}


pub fn load_sample<'a>(file_loc: &str, word_to_id: &BTreeMap<String, u32>) -> (Vec<Vec<QueryWord>>, Vec<Vec<QueryWord>>) {
    let f = File::open(file_loc).expect("tried to open_file");
    let file_buf = BufReader::new(&f);
    let mut rng = thread_rng();
    let mut sample_full: Vec<Vec<QueryWord>> = vec![];
    let mut sample_prefix: Vec<Vec<QueryWord>> = vec![];
    for line in file_buf.lines() {
       let s: String = line.unwrap();
       let mut word_ids: Vec<u32> = vec![];
       let words = tokenize(s.as_str());
       for word in words.iter() {
           let word_id = word_to_id.get(word).unwrap();
           word_ids.push(*word_id);
       }
       let query_words_full = word_ids.iter()
           .map(|w| QueryWord::Full{ id: *w, edit_distance: 0})
           .collect::<Vec<QueryWord>>();

       let query_length = rng.gen_range(0, word_ids.len()-1);
       let last_word = &words[query_length];
       let prefix_length = rng.gen_range(1, last_word.len());
       let prefix = &last_word[..prefix_length];
       let mut prefix_range = word_to_id.range::<String, _>(prefix.to_string()..)
           .take_while(|(&k, &v)| { k.starts_with(&prefix) });
       let prefix_id_min = match prefix_range.next() {
           Some((ref k, ref v)) => **v,
           _ => panic!("Prefix '{:?}' has no match in word_to_id", prefix),
       };
       let prefix_id_max = match prefix_range.last() {
           Some((ref k, ref v)) => **v,
           None => prefix_id_min
       };

       let mut query_words_prefix = Vec::from(&query_words_full[..query_length]);
       query_words_prefix.push(QueryWord::Prefix{ id_range: ( prefix_id_min, prefix_id_max) });

       sample_full.push(query_words_full);
       sample_prefix.push(query_words_prefix);
    }
    // we want to randomly sample so that we get lots of different results
    rng.shuffle(&mut sample_full);
    rng.shuffle(&mut sample_prefix);
    return (sample_full, sample_prefix)
}


pub fn benchmark(c: &mut Criterion) {
    // the things I'm going to set up once and share across benchmarks are a list of words
    // and a built prefix set, so define a struct to contain them
    struct BenchData {
        word_to_id: BTreeMap<String, u32>,
        sample_full: Vec<Vec<QueryWord>>,
        sample_prefix: Vec<Vec<QueryWord>>,
        phrase_set: PhraseSet
    };
    let data_basename = match env::var("PHRASE_BENCH") {
        Ok(f) => {
            println!("file loc is {}", f);
            f
        },
        Err(..) => String::from("./benches/data/phrase_test"),
    };
    let data_loc = format!("{}.txt", data_basename);
    let (word_to_id, phrase_set) = build_phrase_graph(&data_loc);


    let sample_loc = format!("{}_sample.txt", data_basename);
    let (mut sample_full, mut sample_prefix) = load_sample(&sample_loc, &word_to_id);


    // move the prebuilt data into a reference-counted struct
    let shared_data = Rc::new(BenchData { word_to_id, sample_full, sample_prefix, phrase_set });

    // make a vector I'm going to fill with closures to bench-test
    let mut to_bench = Vec::new();

    // each closure gets its own copy of the prebuilt data, but the "copy" is cheap since it's an
    // RC -- this is just a new reference and an increment to the count
    //
    // the copy will the get moved into the closure, but the original will stick around to be
    // copied for the next one
    let data = shared_data.clone();

    to_bench.push(Fun::new("exact_contains", move |b: &mut Bencher, _i| {
        let mut cycle = data.sample_full.iter().cycle();

        // the closure based to b.iter is the thing that will actually be timed; everything before
        // that is untimed per-benchmark setup
        b.iter(|| {
            let query_words = cycle.next().unwrap();
            let query_phrase = QueryPhrase::new(&query_words).unwrap();
            let _result = data.phrase_set.contains(query_phrase).unwrap();
        });
    }));

    // data is shadowed here for ease of copying and pasting, but this is a new clone
    // (again, same data, new reference, because it's an Rc)
    let data = shared_data.clone();
    to_bench.push(Fun::new("exact_contains_prefix", move |b: &mut Bencher, _i| {
        let mut cycle = data.sample_full.iter().cycle();

        b.iter(|| {
            let query_words = cycle.next().unwrap();
            let query_phrase = QueryPhrase::new(&query_words).unwrap();
            let _result = data.phrase_set.contains_prefix(query_phrase).unwrap();
        });
    }));

    // // data is shadowed here for ease of copying and pasting, but this is a new clone
    // // (again, same data, new reference, because it's an Rc)
    // let data = shared_data.clone();
    // to_bench.push(Fun::new("range_contains_prefix", move |b: &mut Bencher, _i| {
    //     let mut cycle = data.sample.iter().cycle();

    //     b.iter(|| {
    //         let word_ids = cycle.next().unwrap();
    //         let fullword_ids = &word_ids[..word_ids.len()];
    //         let last_id = &word_ids[word_ids.len()-1];
    //         let last_id_min = 0.max(last_id - 50);
    //         let last_id_max = last_id + 50;
    //         let mut query_words = fullword_ids.iter()
    //             .map(|w| QueryWord::Full{ id: *w, edit_distance: 0})
    //             .collect::<Vec<QueryWord>>();
    //         query_words.push(QueryWord::Prefix{ id_range: (last_id_min, last_id_max) });
    //         let query_phrase = QueryPhrase::new(&query_words).unwrap();
    //         let _result = data.phrase_set.contains_prefix(query_phrase).unwrap();
    //     });
    // }));

    // // data is shadowed here for ease of copying and pasting, but this is a new clone
    // // (again, same data, new reference, because it's an Rc)
    // let data = shared_data.clone();
    // to_bench.push(Fun::new("range_fst_range", move |b: &mut Bencher, _i| {
    //     let mut cycle = data.sample.iter().cycle();

    //     b.iter(|| {
    //         let word_ids = cycle.next().unwrap();
    //         let fullword_ids = &word_ids[..word_ids.len()];
    //         let last_id = &word_ids[word_ids.len()-1];
    //         let last_id_min = 0.max(last_id - 50);
    //         let last_id_max = last_id + 50;
    //         let mut query_words = fullword_ids.iter()
    //             .map(|w| QueryWord::Full{ id: *w, edit_distance: 0})
    //             .collect::<Vec<QueryWord>>();
    //         query_words.push(QueryWord::Prefix{ id_range: (last_id_min, last_id_max) });
    //         let query_phrase = QueryPhrase::new(&query_words).unwrap();
    //         let _result = data.phrase_set.range(query_phrase).unwrap();
    //     });
    // }));

    // run the accumulated list of benchmarks
    c.bench_functions("phrase", to_bench, ());
}
