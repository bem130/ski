// main.rs

use clap::Parser; // Use the actual name "Parser" as expected by the derive macro
use std::collections::HashMap;
use std::fs;
use std::error::Error;

/// Command line arguments structure using clap's Parser derive.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input file containing the definitions and final SK combinator expression
    #[arg(short = 'i', long = "input")]
    input: String,
}

/// Enum representing the SK combinator expressions, including variables.
#[derive(Clone, Debug)]
enum Expr {
    S,
    K,
    Var(String),
    // Application of two expressions.
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    /// Convert the expression to a string.
    fn to_string(&self) -> String {
        match self {
            Expr::S => "S".to_string(),
            Expr::K => "K".to_string(),
            Expr::Var(name) => name.clone(),
            Expr::App(a, b) => {
                // Parenthesize subexpressions if necessary.
                let a_str = match **a {
                    Expr::App(_, _) => format!("({})", a.to_string()),
                    _ => a.to_string(),
                };
                let b_str = match **b {
                    Expr::App(_, _) => format!("({})", b.to_string()),
                    _ => b.to_string(),
                };
                format!("{} {}", a_str, b_str)
            }
        }
    }
}

/// A simple recursive descent parser for SK combinator expressions and variables.
struct SKParser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> SKParser<'a> {
    /// Create a new parser instance.
    fn new(input: &'a str) -> Self {
        SKParser { input, pos: 0 }
    }

    /// Parse an expression, which is one or more terms (left-associative application).
    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;
        // Parse additional terms for left-associative application.
        while let Some(_) = self.peek_non_space() {
            if let Ok(term) = self.parse_term() {
                expr = Expr::App(Box::new(expr), Box::new(term));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse a single term: either a combinator, variable, or a parenthesized expression.
    fn parse_term(&mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        if self.pos >= self.input.len() {
            return Err("Unexpected end of input".to_string());
        }
        let c = self.current_char();
        match c {
            // Recognize S (case-insensitive)
            'S' => {
                self.pos += 1;
                Ok(Expr::S)
            },
            // Recognize K (case-insensitive)
            'K' => {
                self.pos += 1;
                Ok(Expr::K)
            },
            // Parenthesized expression.
            '(' => {
                self.pos += 1; // skip '('
                let expr = self.parse_expr()?;
                self.skip_whitespace();
                if self.pos >= self.input.len() || self.current_char() != ')' {
                    return Err("Expected ')'".to_string());
                }
                self.pos += 1; // skip ')'
                Ok(expr)
            },
            _ => {
                // If the character is alphabetic, parse it as an identifier (variable).
                if c.is_alphabetic() {
                    let start = self.pos;
                    while self.pos < self.input.len() {
                        let ch = self.input[self.pos..].chars().next().unwrap();
                        if ch.is_alphanumeric() {
                            self.pos += ch.len_utf8();
                        } else {
                            break;
                        }
                    }
                    let var_name = self.input[start..self.pos].to_string();
                    Ok(Expr::Var(var_name))
                } else {
                    Err(format!("Unexpected character: {}", c))
                }
            }
        }
    }

    /// Skip any whitespace characters and inline comments starting with "//".
    fn skip_whitespace(&mut self) {
        loop {
            // If an inline comment is found, skip the rest of the line.
            if self.pos < self.input.len() && self.input[self.pos..].starts_with("//") {
                self.pos = self.input.len();
                break;
            }
            if self.pos < self.input.len() {
                let ch = self.input[self.pos..].chars().next().unwrap();
                if ch.is_whitespace() {
                    self.pos += ch.len_utf8();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Peek the next non-space character without advancing the position.
    fn peek_non_space(&mut self) -> Option<char> {
        let mut pos = self.pos;
        while pos < self.input.len() {
            let remaining = &self.input[pos..];
            if remaining.starts_with("//") {
                return None;
            }
            let ch = remaining.chars().next().unwrap();
            if ch.is_whitespace() {
                pos += ch.len_utf8();
            } else {
                return Some(ch);
            }
        }
        None
    }

    /// Get the current character at the parser's position.
    fn current_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap()
    }
}

/// Reduce an expression by one reduction step, if a redex is found.
/// This function implements normal order reduction by checking for
/// the S and K redex patterns.
fn reduce_expr(expr: &Expr) -> Option<Expr> {
    // Check for S redex: (((S x) y) z) -> ((x z) (y z))
    if let Expr::App(a, z) = expr {
        if let Expr::App(b, y) = &**a {
            if let Expr::App(s, x) = &**b {
                if let Expr::S = **s {
                    return Some(Expr::App(
                        Box::new(Expr::App(Box::new((**x).clone()), Box::new((**z).clone()))),
                        Box::new(Expr::App(Box::new((**y).clone()), Box::new((**z).clone())))
                    ));
                }
            }
        }
    }
    // Check for K redex: ((K x) y) -> x
    if let Expr::App(a, _y) = expr {
        if let Expr::App(k, x) = &**a {
            if let Expr::K = **k {
                return Some((**x).clone());
            }
        }
    }
    // Otherwise, try to reduce subexpressions in normal order.
    match expr {
        Expr::App(f, x) => {
            if let Some(new_f) = reduce_expr(f) {
                return Some(Expr::App(Box::new(new_f), x.clone()));
            }
            if let Some(new_x) = reduce_expr(x) {
                return Some(Expr::App(f.clone(), Box::new(new_x)));
            }
            None
        },
        _ => None,
    }
}

/// Normalize an expression by applying reduction steps until no more redexes exist.
fn normalize(expr: &Expr) -> Expr {
    let mut current = expr.clone();
    loop {
        if let Some(next) = reduce_expr(&current) {
            current = next;
        } else {
            break;
        }
    }
    current
}

/// Recursively substitute defined variables in the expression using the provided definitions.
fn substitute_expr(expr: &Expr, defs: &HashMap<String, Expr>) -> Expr {
    match expr {
        Expr::Var(name) => {
            // If the variable is defined, substitute it recursively.
            if let Some(def_expr) = defs.get(name) {
                substitute_expr(def_expr, defs)
            } else {
                expr.clone()
            }
        },
        Expr::App(f, x) => {
            Expr::App(
                Box::new(substitute_expr(f, defs)),
                Box::new(substitute_expr(x, defs))
            )
        },
        _ => expr.clone(),
    }
}

/// Parse a single definition line of the form "NAME = expression".
fn parse_definition_line(line: &str) -> Result<(String, Expr), String> {
    let parts: Vec<&str> = line.splitn(2, '=').collect();
    if parts.len() != 2 {
        return Err("Invalid definition line; expected format NAME = expression".to_string());
    }
    let name = parts[0].trim();
    if name.is_empty() {
        return Err("Definition name cannot be empty".to_string());
    }
    let mut parser = SKParser::new(parts[1].trim());
    let expr = parser.parse_expr()?;
    Ok((name.to_string(), expr))
}

/// Main function: parses command-line arguments, reads the input file,
/// processes definitions, substitutes them into the final expression, normalizes it, and prints the result.
fn main() -> Result<(), Box<dyn Error>> {
    // Parse command line arguments.
    let args = Args::parse();

    // Read the input file specified by the -i flag.
    let input_content = fs::read_to_string(&args.input)?;

    // Split input into non-empty lines and ignore lines that are pure comments.
    let lines: Vec<&str> = input_content
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !trimmed.is_empty() && !trimmed.starts_with("//")
        })
        .collect();

    if lines.is_empty() {
        return Err("Input file is empty".into());
    }

    // Store definitions in a hashmap.
    let mut defs: HashMap<String, Expr> = HashMap::new();
    // All lines except the final one are treated as definitions.
    for line in &lines[..lines.len().saturating_sub(1)] {
        let (name, expr) = parse_definition_line(line)
            .map_err(|e| format!("Definition parse error: {}", e))?;
        defs.insert(name, expr);
    }

    // Parse the final expression.
    let final_line = lines[lines.len() - 1];
    let mut parser = SKParser::new(final_line);
    let expr = parser
        .parse_expr()
        .map_err(|e| format!("Parse error in final expression: {}", e))?;

    println!("Input expression      : {}", expr.to_string());

    // Substitute defined variables in the final expression.
    let substituted_expr = substitute_expr(&expr, &defs);
    println!("After substitution    : {}", substituted_expr.to_string());

    // Normalize the expression.
    let normalized_expr = normalize(&substituted_expr);
    println!("Normalized expression : {}", normalized_expr.to_string());

    Ok(())
}
