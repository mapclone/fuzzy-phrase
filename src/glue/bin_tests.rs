extern crate lazy_static;

use fst::raw::Output;
use super::bins::PrefixBin;
use super::{FuzzyPhraseSetBuilder, FuzzyPhraseSet};

lazy_static! {
    static ref DIR: tempfile::TempDir = tempfile::tempdir().unwrap();
    static ref CITIES: Vec<&'static str> = vec![
        "raleigh north carolina",
        "rancho cucamonga california",
        "reno nevada",
        "renton washington",
        "rialto california",
        "richardson texas",
        "richmond california",
        "richmond virginia",
        "riverside california",
        "rochester minnesota",
        "rochester new york",
        "rockford illinois",
        "roseville california",
        "round rock texas",
        "sacramento california",
        "saint paul minnesota",
        "salem oregon",
        "salinas california",
        "salt lake city utah",
        "san angelo texas",
        "san antonio texas",
        "san bernardino california",
        "san diego california",
        "san francisco california",
        "san jose california",
        "san mateo california",
        "sandy springs georgia",
        "santa ana california",
        "santa clara california",
        "santa clarita california",
        "santa maria california",
        "santa rosa california",
        "savannah georgia",
        "scottsdale arizona",
        "seattle washington",
        "shreveport louisiana",
        "simi valley california",
        "sioux falls south dakota",
        "south bend indiana",
        "sparks nevada",
        "spokane washington",
        "springfield illinois",
        "springfield massachusetts",
        "springfield missouri",
        "st louis missouri",
        "st petersburg florida",
        "stamford connecticut",
        "sterling heights michigan",
        "stockton california",
        "sugar land texas",
        "sunnyvale california",
        "surprise arizona",
        "syracuse new york",
    ];
    static ref SET: FuzzyPhraseSet = {
        let mut builder = FuzzyPhraseSetBuilder::new(&DIR.path()).unwrap();
        for phrase in CITIES.iter() {
            builder.insert_str(phrase).unwrap();
        }
        builder.finish().unwrap();
        FuzzyPhraseSet::from_path(&DIR.path()).unwrap()
    };
}

fn range_for_prefix(prefix: &str) -> Option<PrefixBin> {
    let mut bounds = None;
    for (i, phrase) in CITIES.iter().enumerate() {
        if phrase.starts_with(prefix) {
            bounds = match bounds {
                None => Some((i, i)),
                Some((j, _)) => Some((j, i))
            };
        }
    }
    bounds.map(|(first, last)| PrefixBin {
        prefix: prefix.trim().to_string(),
        first: Output::new(first as u64),
        last: Output::new(last as u64),
        size: last - first + 1
    })
}

#[test]
fn unlimited_bins() {
    assert_eq!(
        SET.get_prefix_bins(std::usize::MAX).unwrap(),
        vec![range_for_prefix("r").unwrap(), range_for_prefix("s").unwrap()]
    );
}

#[test]
fn simple_division() {
    // r's stay together, s's have to get subidivided by one letter
    assert_eq!(
        SET.get_prefix_bins(20).unwrap(),
        vec![
            range_for_prefix("r").unwrap(),
            range_for_prefix("sa").unwrap(),
            range_for_prefix("sc").unwrap(),
            range_for_prefix("se").unwrap(),
            range_for_prefix("sh").unwrap(),
            range_for_prefix("si").unwrap(),
            range_for_prefix("so").unwrap(),
            range_for_prefix("sp").unwrap(),
            range_for_prefix("st").unwrap(),
            range_for_prefix("su").unwrap(),
            range_for_prefix("sy").unwrap(),
        ]
    );
}

#[test]
fn reach_word_boundaries() {
    // r's get split, some s's get further split, and the san's cross the word boundary
    assert_eq!(
        SET.get_prefix_bins(7).unwrap(),
        vec![
            range_for_prefix("ra").unwrap(),
            range_for_prefix("re").unwrap(),
            range_for_prefix("ri").unwrap(),
            range_for_prefix("ro").unwrap(),
            range_for_prefix("sac").unwrap(),
            range_for_prefix("sai").unwrap(),
            range_for_prefix("sal").unwrap(),
            range_for_prefix("san ").unwrap(),
            range_for_prefix("sand").unwrap(),
            range_for_prefix("sant").unwrap(),
            range_for_prefix("sav").unwrap(),
            range_for_prefix("sc").unwrap(),
            range_for_prefix("se").unwrap(),
            range_for_prefix("sh").unwrap(),
            range_for_prefix("si").unwrap(),
            range_for_prefix("so").unwrap(),
            range_for_prefix("sp").unwrap(),
            range_for_prefix("st").unwrap(),
            range_for_prefix("su").unwrap(),
            range_for_prefix("sy").unwrap(),
        ]
    );
}

#[test]
fn cross_word_boundaries() {
    // r's get split, some s's get further split, and the san's cross the word boundary
    assert_eq!(
        SET.get_prefix_bins(5).unwrap(),
        vec![
            range_for_prefix("ra").unwrap(),
            range_for_prefix("re").unwrap(),
            range_for_prefix("ri").unwrap(),
            range_for_prefix("ro").unwrap(),
            range_for_prefix("sac").unwrap(),
            range_for_prefix("sai").unwrap(),
            range_for_prefix("sal").unwrap(),
            range_for_prefix("san a").unwrap(),
            range_for_prefix("san b").unwrap(),
            range_for_prefix("san d").unwrap(),
            range_for_prefix("san f").unwrap(),
            range_for_prefix("san j").unwrap(),
            range_for_prefix("san m").unwrap(),
            range_for_prefix("sand").unwrap(),
            range_for_prefix("sant").unwrap(),
            range_for_prefix("sav").unwrap(),
            range_for_prefix("sc").unwrap(),
            range_for_prefix("se").unwrap(),
            range_for_prefix("sh").unwrap(),
            range_for_prefix("si").unwrap(),
            range_for_prefix("so").unwrap(),
            range_for_prefix("sp").unwrap(),
            range_for_prefix("st").unwrap(),
            range_for_prefix("su").unwrap(),
            range_for_prefix("sy").unwrap(),
        ]
    );
}

#[test]
fn minimal_bins() {
    // prefixes are now as long as they need to be to uniquely identify a single phrase
    assert_eq!(
        SET.get_prefix_bins(1).unwrap(),
        vec![
            range_for_prefix("ral").unwrap(),
            range_for_prefix("ran").unwrap(),
            range_for_prefix("reno").unwrap(),
            range_for_prefix("rent").unwrap(),
            range_for_prefix("ria").unwrap(),
            range_for_prefix("richa").unwrap(),
            range_for_prefix("richmond c").unwrap(),
            range_for_prefix("richmond v").unwrap(),
            range_for_prefix("riv").unwrap(),
            range_for_prefix("rochester m").unwrap(),
            range_for_prefix("rochester n").unwrap(),
            range_for_prefix("rock").unwrap(),
            range_for_prefix("ros").unwrap(),
            range_for_prefix("rou").unwrap(),
            range_for_prefix("sac").unwrap(),
            range_for_prefix("sai").unwrap(),
            range_for_prefix("sale").unwrap(),
            range_for_prefix("sali").unwrap(),
            range_for_prefix("salt").unwrap(),
            range_for_prefix("san ang").unwrap(),
            range_for_prefix("san ant").unwrap(),
            range_for_prefix("san b").unwrap(),
            range_for_prefix("san d").unwrap(),
            range_for_prefix("san f").unwrap(),
            range_for_prefix("san j").unwrap(),
            range_for_prefix("san m").unwrap(),
            range_for_prefix("sand").unwrap(),
            range_for_prefix("santa a").unwrap(),
            range_for_prefix("santa clara").unwrap(),
            range_for_prefix("santa clari").unwrap(),
            range_for_prefix("santa m").unwrap(),
            range_for_prefix("santa r").unwrap(),
            range_for_prefix("sav").unwrap(),
            range_for_prefix("sc").unwrap(),
            range_for_prefix("se").unwrap(),
            range_for_prefix("sh").unwrap(),
            range_for_prefix("sim").unwrap(),
            range_for_prefix("sio").unwrap(),
            range_for_prefix("so").unwrap(),
            range_for_prefix("spa").unwrap(),
            range_for_prefix("spo").unwrap(),
            range_for_prefix("springfield i").unwrap(),
            range_for_prefix("springfield ma").unwrap(),
            range_for_prefix("springfield mi").unwrap(),
            range_for_prefix("st l").unwrap(),
            range_for_prefix("st p").unwrap(),
            range_for_prefix("sta").unwrap(),
            range_for_prefix("ste").unwrap(),
            range_for_prefix("sto").unwrap(),
            range_for_prefix("sug").unwrap(),
            range_for_prefix("sun").unwrap(),
            range_for_prefix("sur").unwrap(),
            range_for_prefix("sy").unwrap(),
        ]
    );
}