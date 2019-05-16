use super::util;
use super::WordKey;

/// An abstraction over full words and prefixes.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum QueryWord {
    /// A `Full` word is a word that has an identifier and is one of the members of a PrefixSet.
    Full {
        id: u32,
        key: WordKey,
        edit_distance: u8,
    },

    /// A `Prefix` is a string that is the prefix to more than one full word, and includes an id_range field,
    /// which of identifiers.
    Prefix {
        id_range: (u32, u32),
        key_range: (WordKey, WordKey),
    },
}

impl QueryWord
{

    pub fn new_full(id:u32, edit_distance:u8) -> QueryWord {
        let key: [u8; 3] = util::three_byte_encode(id);
        QueryWord::Full { id, edit_distance, key }
    }

    pub fn new_prefix(id_range: (u32, u32)) -> QueryWord {
        let min_key: [u8; 3] = util::three_byte_encode(id_range.0);
        let max_key: [u8; 3] = util::three_byte_encode(id_range.1);
        let key_range = (min_key, max_key);
        QueryWord::Prefix { id_range, key_range }
    }

    pub fn to_string<'a, T:Fn(u32) -> &'a str>(&self, id_to_string: T) -> String {
        match &self {
            &QueryWord::Full {id, ..} => {
                let s = format!("{}", id_to_string(*id));
                return s
            },
            &QueryWord::Prefix {id_range, ..} => {
                let s_start: &str = id_to_string(id_range.0);
                let s_end: &str = id_to_string(id_range.1);
                let s = format!("{}..{}", s_start, s_end);
                return s
            }
        }
    }
}

impl Default for QueryWord {
    fn default() -> QueryWord {
        QueryWord::Full {
            id: 0,
            key: [255u8, 255u8, 255u8],
            edit_distance: 99,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn query_word_partial_eq() {
        let word =  QueryWord::new_full(1u32, 0);
        let matching_word = QueryWord::new_full(1u32, 0);
        let nonmatching_word = QueryWord::new_full(2u32, 0);

        assert!(word != nonmatching_word);
        assert!(word == matching_word);

        let prefix = QueryWord::new_prefix((561_528u32, 561_531u32));
        let matching_prefix = QueryWord::new_prefix((561_528u32, 561_531u32));
        let nonmatching_prefix = QueryWord::new_prefix((1u32, 561_531u32));

        assert!(word != prefix);
        assert!(prefix == matching_prefix);
        assert!(prefix != nonmatching_prefix);
    }

    #[test]
    fn query_word_to_string() {
        let mut id_to_string_map = HashMap::new();

        id_to_string_map.insert(1u32, String::from("100"));
        id_to_string_map.insert(61_528u32, String::from("main"));
        id_to_string_map.insert(561_528u32, String::from("st"));

        let query_word = QueryWord::new_full(61_528u32, 0);

        let id_to_string_closure = |id: u32| id_to_string_map.get(&id).unwrap().as_str();

        let s = query_word.to_string(id_to_string_closure);
        assert_eq!(String::from("main"), s);
    }
}
