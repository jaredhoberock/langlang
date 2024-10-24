use std::io::Write;

use langlang::interpreter::Interpreter;
use langlang::lexer::Lexer;
use langlang::parser::parse_program;
use langlang::token::Token;

fn usage() {
    println!("usage: langlang [script]");
}

fn interpret(interp: &mut Interpreter, source: &str) -> bool {
    let tokens: Vec<Token> = Lexer::new(source).collect();

    let prog = match parse_program(&tokens) {
        Ok(prog) => prog,
        Err(error) => {
            eprintln!("Syntax error: {}", error);
            return false;
        }
    };

    match interp.interpret_program(&prog) {
        Ok(_) => true,
        Err(error) => {
            eprintln!("Runtime error: {}", error);
            false
        }
    }
}

fn interpret_from_file(filename: &str) -> bool {
    match std::fs::read_to_string(filename) {
        Ok(source) => {
            let mut interp = Interpreter::new();
            interpret(&mut interp, &source)
        },
        Err(error) => {
            eprintln!("Error reading file: {}", error);
            false
        }
    }
}

fn interpret_from_prompt() -> bool {
    let mut interp = Interpreter::new();
    let stdin = std::io::stdin();
    let mut input = String::new();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        // clear previous input
        input.clear();

        match stdin.read_line(&mut input) {
            Ok(0) => return true, // EOF
            Ok(_) => interpret(&mut interp, &input),
            Err(error) => {
                eprintln!("Error reading line: {}", error);
                return false;
            }
        };
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 2 {
        usage();
        return;
    }

    let result = if args.len() == 2 {
        interpret_from_file(&args[1])
    } else {
        interpret_from_prompt()
    };

    std::process::exit(if result { 0 } else { -1 });
}
