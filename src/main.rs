// main.rs

// Use clap for command-line argument parsing with derive macros.
use clap::Parser; // Use the actual name "Parser" as expected by the derive macro
use std::fs;
use std::error::Error;

/// Command line arguments structure using clap's Parser derive.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input file containing the SK combinator expression
    #[arg(short = 'i', long = "input")]
    input: String,
}

/// Enum representing the SK combinator expressions.
#[derive(Clone, Debug)]
enum Expr {
    S,
    K,
    // Application of two expressions.
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    /// Convert the expression to a string.
    fn to_string(&self) -> String {
        match self {
            Expr::S => "S".to_string(),
            Expr::K => "K".to_string(),
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

/// A simple recursive descent parser for SK combinator expressions.
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
            // Attempt to parse the next term; break if it fails.
            if let Ok(term) = self.parse_term() {
                expr = Expr::App(Box::new(expr), Box::new(term));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse a single term: either a combinator or a parenthesized expression.
    fn parse_term(&mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        if self.pos >= self.input.len() {
            return Err("Unexpected end of input".to_string());
        }
        let c = self.current_char();
        match c {
            // Recognize S (case-insensitive)
            'S' | 's' => {
                self.pos += 1;
                Ok(Expr::S)
            },
            // Recognize K (case-insensitive)
            'K' | 'k' => {
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
            _ => Err(format!("Unexpected character: {}", c)),
        }
    }

    /// Skip any whitespace characters.
    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() && self.input[self.pos..].chars().next().unwrap().is_whitespace() {
            self.pos += 1;
        }
    }

    /// Peek the next non-space character without advancing the position.
    fn peek_non_space(&mut self) -> Option<char> {
        let mut pos = self.pos;
        while pos < self.input.len() {
            let ch = self.input[pos..].chars().next().unwrap();
            if !ch.is_whitespace() {
                return Some(ch);
            }
            pos += 1;
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
                    // Apply the S rule: substitute and build the new expression.
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

/// Main function: parses command-line arguments, reads the input file,
/// parses the SK combinator expression, reduces it to normal form, and prints the result.
fn main() -> Result<(), Box<dyn Error>> {
    // Parse command line arguments.
    let args = Args::parse();

    // Read the input file specified by the -i flag.
    let input_content = fs::read_to_string(&args.input)?;
    
    // Create a parser for the input content using our SKParser.
    let mut parser = SKParser::new(&input_content);
    // Parse the expression; if there's an error, report it.
    let expr = parser.parse_expr().map_err(|e| format!("Parse error: {}", e))?;
    
    println!("Input expression: {}", expr.to_string());
    
    // Normalize the expression by applying reduction rules repeatedly.
    let normalized_expr = normalize(&expr);
    
    println!("Normalized expression: {}", normalized_expr.to_string());
    
    Ok(())
}
