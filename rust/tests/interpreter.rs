use std::fs;
use std::path::Path;
use std::process::Command;
use pretty_assertions::assert_eq;

/// This function tests the interpreter by running it on a source file and comparing the output
/// to the expected output in the corresponding `.expected` file.
fn test_interpreter(source_file: &Path, expected_output_file: &Path) {
    // Read the expected output from the `.expected` file
    let expected_output = fs::read_to_string(expected_output_file)
        .expect("Failed to read expected output file");

    // Run the interpreter on the `.lox` file using `cargo run`
    let output = Command::new("cargo")
        .arg("run")
        .arg("--quiet")
        .arg("--")
        .arg(source_file.to_str().unwrap())  // Pass the source file as input to the interpreter
        .output()
        .expect("Failed to run interpreter");

    // Convert the interpreter's output from bytes to a UTF-8 string
    let interpreter_output = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    // Compare the interpreter output with the expected output
    assert_eq!(
        interpreter_output.trim(),
        expected_output.trim(),
        "Test failed for: {:?}",
        source_file
    );
}

#[test]
fn test_all_lox_files() {
    // Define the directory where `.lox` files are located
    let test_dir = Path::new("tests/lox");

    // Iterate over all the entries in the `tests/lox` directory
    for entry in fs::read_dir(test_dir).expect("Failed to read test directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        // Check if the file has a `.lox` extension
        if path.extension().and_then(|s| s.to_str()) == Some("lox") {
            // Determine the corresponding `.expected` file
            let expected_output_file = path.with_extension("expected");

            // Run the interpreter test on the `.lox` file and its corresponding `.expected` file
            test_interpreter(&path, &expected_output_file);
        }
    }
}
