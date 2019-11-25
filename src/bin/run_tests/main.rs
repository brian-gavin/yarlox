extern crate ansi_term;
use std::thread;

mod test_runner;
mod tests;
use test_runner::test_runner_work;
use tests::collect_tests;

fn main() {
    let tests = collect_tests().expect("Error collecting tests");
    let runners: Vec<thread::JoinHandle<_>> = tests
        .into_iter()
        .map(|test| thread::spawn(move || test_runner_work(test)))
        .collect();
    for runner in runners.into_iter() {
        match runner.join().expect("Test runner failed to join") {
            Ok(success) => println!("{}", success),
            Err(error) => eprintln!("{}", error),
        }
    }
}
