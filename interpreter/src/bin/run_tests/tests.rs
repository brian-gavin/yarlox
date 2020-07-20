use std::collections::HashMap;
use std::fs;
use std::io::{self, prelude::*};
use std::path::{Path, PathBuf};

const TEST_PATH: &'static str = "./tests";

#[derive(Debug)]
pub struct TestPair {
    pub test_program: PathBuf,
    pub test_out: PathBuf,
}

#[derive(Default, Debug)]
struct PartialTestPair {
    test_program: Option<PathBuf>,
    test_out: Option<PathBuf>,
}

impl PartialTestPair {
    fn add_path(&mut self, path: &Path) {
        if let Some(extension) = path.extension().and_then(|s| s.to_str()) {
            if extension == "lox" {
                self.test_program = Some(PathBuf::from(path));
            } else if extension == "out" {
                self.test_out = Some(PathBuf::from(path));
            }
        }
    }
}

impl TestPair {
    fn from_partial(partial: PartialTestPair) -> Option<TestPair> {
        if partial.test_out.is_none() || partial.test_program.is_none() {
            None
        } else {
            Some(TestPair {
                test_program: partial.test_program.unwrap(),
                test_out: partial.test_out.unwrap(),
            })
        }
    }

    pub fn test_name(&self) -> &str {
        self.test_program
            .file_name()
            .expect("Test program has no filename")
            .to_str()
            .expect("Test program has invalid UTF-8 path")
    }

    pub fn read_out_file(&self) -> io::Result<String> {
        let mut file = fs::File::open(self.test_out.as_path())?;
        let mut output = String::new();
        file.read_to_string(&mut output)?;
        Ok(output)
    }
}

pub fn collect_tests() -> io::Result<Vec<TestPair>> {
    let name_to_pairs_map = get_names_to_partial_pairs_map()?;
    Ok(name_to_pairs_map
        .into_iter()
        .map(|(_k, v)| v)
        .filter_map(TestPair::from_partial)
        .collect())
}

fn get_test_name(path: &Path) -> Option<String> {
    path.file_stem()
        .and_then(|os_str| os_str.to_str())
        .map(|s| s.trim_end_matches("_test"))
        .map(|s| s.to_string())
}

fn get_names_to_partial_pairs_map() -> io::Result<HashMap<String, PartialTestPair>> {
    let mut name_to_pairs_map: HashMap<String, PartialTestPair> = HashMap::new();
    for entry in fs::read_dir(TEST_PATH)? {
        let entry = entry?;
        let path = entry.path();
        let test_name = if let Some(test_name) = get_test_name(&path) {
            test_name
        } else {
            continue;
        };
        if let Some(partial_test_pair) = name_to_pairs_map.get_mut(&test_name) {
            partial_test_pair.add_path(&path);
        } else {
            let mut pair = PartialTestPair::default();
            pair.add_path(&path);
            name_to_pairs_map.insert(test_name, pair);
        }
    }
    Ok(name_to_pairs_map)
}
