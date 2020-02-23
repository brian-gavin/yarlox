use ansi_term::Color;
use std::io::{self, prelude::*};
use std::process;
use std::thread;
use std::time;
use tests::TestPair;

/// Run in a child thread context!
pub fn test_runner_work(test: TestPair) -> Result<String, String> {
    println!(
        "{}",
        Color::Blue.paint(format!("{}: Running...", test.test_name()))
    );
    let child = process::Command::new("cargo")
        .arg("run")
        .arg(&test.test_program.as_os_str())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .spawn();
    let mut child = match child {
        Ok(c) => c,
        Err(_) => {
            return Err(format!(
                "{}: Could not spawn test runner process",
                test.test_name()
            ));
        }
    };
    // wait for the test to finish
    thread::sleep(time::Duration::from_secs(2));
    // timeout the child if it hasn't exited yet
    let exit_status = match child.try_wait().expect("Failed to wait.") {
        Some(status) => status,
        // the child hasn't exited yet, so need to kill and wait again
        None => {
            child.kill().expect("Failed to kill child.");
            child.wait().expect("Failed to wait.")
        }
    };
    if !exit_status.success() {
        return Err(report_exit_status_error(&test, &mut child).map_err(|e| e.to_string())?);
    }
    let mut out = String::new();
    let expected_out = test.read_out_file().map_err(|e| e.to_string())?;
    child
        .stdout
        .expect("Child stdout was None")
        .read_to_string(&mut out)
        .map_err(|e| e.to_string())?;
    if out == expected_out {
        Ok(report_success(&test))
    } else {
        let mut err = String::new();
        child
            .stderr
            .expect("Child stderr was None")
            .read_to_string(&mut err)
            .map_err(|e| e.to_string())?;
        Err(report_assertion_error(&test, &expected_out, &out, &err))
    }
}

fn report_success(test: &TestPair) -> String {
    Color::Green
        .paint(format!("{}: Success!", test.test_name().to_string()))
        .to_string()
}

fn parse_stderr_lox_error(stderr: &str) -> String {
    stderr
        .lines()
        .filter(|s| s.starts_with('['))
        .collect::<String>()
}

fn report_exit_status_error(test: &TestPair, child: &mut process::Child) -> io::Result<String> {
    let mut child_stderr = String::new();
    child
        .stderr
        .take()
        .expect("Child stderr was None")
        .read_to_string(&mut child_stderr)?;
    Ok(Color::Red
        .paint(format!(
            "{}: FAILED! stderr: {}",
            test.test_name(),
            parse_stderr_lox_error(&child_stderr),
        ))
        .to_string())
}

fn report_assertion_error(test: &TestPair, expected: &str, got: &str, err: &str) -> String {
    Color::Red
        .paint(format!(
            "{}: FAILED! Expected '{}' Got '{}' {{stderr: '{}'}}",
            test.test_name(),
            expected.replace('\n', "\\n"),
            got.replace('\n', "\\n"),
            parse_stderr_lox_error(&err),
        ))
        .to_string()
}
