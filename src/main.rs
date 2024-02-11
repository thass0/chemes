use std::cell::RefCell;
use std::cmp::Ordering;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::rc::Rc;

use chemes::eval;
use chemes::parse;
use chemes::Env;

fn main() {
    let args = {
        let mut args = env::args();
        args.next(); // Skip the executable's name.
        args.collect::<Vec<String>>()
    };

    if args.iter().any(|x| x == "--help" || x == "-h") {
        println!("{HELP_MSG}");
        return;
    }

    let env = Rc::new(RefCell::new(Env::new()));
    match args.get(0) {
        Some(filename) => {
            let file = File::open(filename).unwrap_or_else(|_| panic!("failed to open {filename}"));
            let reader = BufReader::new(file);
            let lines = reader.lines().map(|l| l.expect("failed to read line"));
            run_lines(env, lines);
        }
        None => {
            let stdin = io::stdin();
            let lines = stdin
                .lock()
                .lines()
                .map(|l| l.expect("failed to read line"));
            run_lines(env, lines);
        }
    }
}

fn run_lines<I>(env: Rc<RefCell<Env>>, lines: I)
where
    I: Iterator<Item = String>,
{
    let mut buf = String::new();
    let mut balance = 0;

    for line in lines {
        let mut tmp_balance = balance;
        for ch in line.chars() {
            match ch {
                '(' => tmp_balance += 1,
                ')' => tmp_balance -= 1,
                _ => {}
            }
        }

        match tmp_balance.cmp(&0) {
            Ordering::Less => {
                eprintln!("ignored last line because it has too many closing parenthesis");
            }
            Ordering::Greater => {
                // Wait until all open parentheses are closed.
                if !buf.is_empty() {
                    buf.push(' ');
                }
                buf.push_str(&line);
                balance = tmp_balance;
            }
            Ordering::Equal => {
                if !buf.is_empty() {
                    buf.push(' ');
                }
                buf.push_str(&line);
                balance = tmp_balance;

                match parse(&buf) {
                    Ok(values) => {
                        for value in values {
                            match eval(env.clone(), value) {
                                Ok(v) => println!("{v}"),
                                Err(e) => eprintln!("{e:?}"),
                            };
                        }
                    }
                    Err(e) => {
                        eprintln!("{e:?}");
                    }
                }

                buf.clear();
            }
        }
    }
}

const HELP_MSG: &str = r#"Usage: chemes [FILE] [--help|-h]

  FILE  Name of a Scheme source file to evaluate

Options:
  -h, --help  Print this message"#;
