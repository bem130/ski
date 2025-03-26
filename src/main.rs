// main.rs

use clap::Parser;
use std::collections::HashMap;
use std::fs;
use std::error::Error;

/// Enum representing the SK combinator expressions, including variables.
#[derive(Clone, Debug, PartialEq)]
enum Expr {
    S,
    K,
    Var(String),
    // Application of two expressions.
    App(Box<Expr>, Box<Expr>),
}

/// New enum for displaying expressions with diff and variable definition coloring.
#[derive(Clone, Debug, PartialEq)]
enum DisplayExpr {
    S,
    K,
    UndefinedVar(String),
    DefinedVar(String),
    // Application of two expressions.
    App(Box<DisplayExpr>, Box<DisplayExpr>),
    // For diff display: extra part in one expression.
    Excess(Box<DisplayExpr>),
    // For diff display: missing part (displayed as underscore).
    Lack,
}

/// Convert an Expr to a DisplayExpr, taking into account variable definitions.
/// If the variable is defined in defs, mark it as DefinedVar, otherwise UndefinedVar.
fn to_display_expr(expr: &Expr, defs: &HashMap<String, Expr>) -> DisplayExpr {
    match expr {
        Expr::S => DisplayExpr::S,
        Expr::K => DisplayExpr::K,
        Expr::Var(name) => {
            if defs.contains_key(name) {
                DisplayExpr::DefinedVar(name.clone())
            } else {
                DisplayExpr::UndefinedVar(name.clone())
            }
        },
        Expr::App(a, b) => DisplayExpr::App(
            Box::new(to_display_expr(a, defs)),
            Box::new(to_display_expr(b, defs))
        ),
    }
}

fn diff_exprs_sym(lhs: &DisplayExpr, rhs: &DisplayExpr) -> (DisplayExpr, DisplayExpr) {
    if lhs == rhs {
        return (lhs.clone(), rhs.clone());
    }
    match (lhs, rhs) {
        (DisplayExpr::App(l1, l2), DisplayExpr::App(r1, r2)) => {
            let (d1_l, d1_r) = diff_exprs_sym(l1, r1);
            let (d2_l, d2_r) = diff_exprs_sym(l2, r2);
            return (
                DisplayExpr::App(Box::new(d1_l), Box::new(d2_l)),
                DisplayExpr::App(Box::new(d1_r), Box::new(d2_r))
            );
        },
        _ => {
            // For lhs: mark its extra content in green (Excess) and indicate missing with a red underscore (Lack).
            // For rhs: show a red underscore first then its extra content in green.
            let lhs_diff = DisplayExpr::Excess(Box::new(lhs.clone()));
            let rhs_diff = DisplayExpr::Excess(Box::new(rhs.clone()));
            return (
                DisplayExpr::App(Box::new(lhs_diff), Box::new(DisplayExpr::Lack)),
                DisplayExpr::App(Box::new(DisplayExpr::Lack), Box::new(rhs_diff))
            );
        }
    }
}

/// to_highlighted_string implementation for DisplayExpr.
/// It handles highlighting for diff (Excess, Lack) and variable definition coloring.
impl DisplayExpr {
    fn to_highlighted_string(&self, mode: &highlight::HighlightMode, depth: usize) -> String {
        match self {
            DisplayExpr::S => format!("{}S{}", highlight::colors::pink(mode), highlight::reset(mode)),
            DisplayExpr::K => format!("{}K{}", highlight::colors::pink(mode), highlight::reset(mode)),
            DisplayExpr::UndefinedVar(name) => format!("{}{}{}", highlight::colors::lightblue(mode), name, highlight::reset(mode)),
            DisplayExpr::DefinedVar(name) => format!("{}{}{}", highlight::colors::orange(mode), name, highlight::reset(mode)),
            DisplayExpr::App(a, b) => {
                let a_str = match **a {
                    DisplayExpr::App(_, _) => {
                        // Wrap in parentheses with color based on depth.
                        format!("{}({}{}){}", 
                            highlight::paren_color(depth, mode), 
                            a.to_highlighted_string(mode, depth + 1), 
                            highlight::paren_color(depth, mode), 
                            highlight::reset(mode))
                    },
                    _ => a.to_highlighted_string(mode, depth),
                };
                let b_str = match **b {
                    DisplayExpr::App(_, _) => {
                        format!("{}({}{}){}", 
                            highlight::paren_color(depth, mode), 
                            b.to_highlighted_string(mode, depth + 1), 
                            highlight::paren_color(depth, mode), 
                            highlight::reset(mode))
                    },
                    _ => b.to_highlighted_string(mode, depth),
                };
                format!("{} {}", a_str, b_str)
            },
            DisplayExpr::Excess(inner) => {
                // Green background for extra parts
                format!("{}{}{}",  highlight::colors::greenbg(mode), inner.to_highlighted_string(mode, depth), highlight::reset(mode))
            },
            DisplayExpr::Lack => {
                // Red background for missing part, shown as underscore
                format!("{}_{}",  highlight::colors::redbg(mode), highlight::reset(mode))
            },
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

    /// Parse an expression (left-associative application).
    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;
        while let Some(_) = self.peek_non_space() {
            if let Ok(term) = self.parse_term() {
                expr = Expr::App(Box::new(expr), Box::new(term));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse a single term: combinator, variable, or parenthesized expression.
    fn parse_term(&mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        if self.pos >= self.input.len() {
            return Err("Unexpected end of input".to_string());
        }
        let c = self.current_char();
        match c {
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
                    match var_name.as_str() {
                        "S" => Ok(Expr::S),
                        "K" => Ok(Expr::K),
                        _ => Ok(Expr::Var(var_name))
                    }
                } else {
                    Err(format!("Unexpected character: {}", c))
                }
            },
        }
    }

    /// Skip whitespace and inline comments.
    fn skip_whitespace(&mut self) {
        loop {
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

    /// Peek next non-space character.
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

    /// Get current character.
    fn current_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap()
    }
}

/// Reduce an expression by one step (normal order reduction).
fn reduce_expr(expr: &Expr) -> Option<Expr> {
    // S redex: (((S x) y) z) -> ((x z) (y z))
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
    // K redex: ((K x) y) -> x
    if let Expr::App(a, _y) = expr {
        if let Expr::App(k, x) = &**a {
            if let Expr::K = **k {
                return Some((**x).clone());
            }
        }
    }
    // Try to reduce subexpressions.
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

/// Normalize expression by applying reduction until no redex is found.
fn normalize(expr: &Expr) -> Expr {
    let mut current = expr.clone();
    while let Some(next) = reduce_expr(&current) {
        current = next;
    }
    current
}

/// Substitute defined variables in the expression.
fn substitute_expr(expr: &Expr, defs: &HashMap<String, Expr>) -> Expr {
    match expr {
        Expr::Var(name) => {
            if let Some(def_expr) = defs.get(name) {
                substitute_expr(def_expr, defs)
            } else {
                expr.clone()
            }
        },
        Expr::App(f, x) => {
            Expr::App(Box::new(substitute_expr(f, defs)), Box::new(substitute_expr(x, defs)))
        },
        _ => expr.clone(),
    }
}

/// Parse a definition line: "NAME = expression".
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

/// Command line arguments.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None, disable_help_flag = true)]
struct Args {
    /// Highlight mode: false, 16, 256, true (no highlight, 16-color, 256-color, 24bit truecolor)
    #[arg(short = 'h', long = "highlight", default_value = "false")]
    highlight: String,

    /// Input file containing definitions, test cases, and final expression.
    #[arg(short = 'i', long = "input")]
    input: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let mode = highlight::HighlightMode::from_str(&args.highlight);
    let input_content = fs::read_to_string(&args.input)?;
    let raw_lines: Vec<(usize, &str)> = input_content
        .lines()
        .enumerate()
        .filter(|(_i, line)| {
            let trimmed = line.trim();
            !trimmed.is_empty() && !trimmed.starts_with("//")
        })
        .collect();

        if raw_lines.is_empty() {
            return Err("Input file is empty".into());
        }

    let mut defs: HashMap<String, Expr> = HashMap::new();
    let mut final_expr_opt: Option<Expr> = None;

    for (line_index, line) in raw_lines {
        let trimmed = line.trim();
        if trimmed.starts_with("$") {
            // Process test case line; include the original line number.
            let test_line = trimmed.trim_start_matches('$').trim();
            let parts: Vec<&str> = test_line.splitn(2, '=').collect();
            if parts.len() != 2 {
                eprintln!("Line {:<5}: {}", line_index + 1, highlight::colorize_plain(
                    &format!("Test parse error: Expected format 'LHS = RHS' in test line: {}", test_line), 
                    "red",
                    &mode
                ));
                continue;
            }
            let lhs_str = parts[0].trim();
            let rhs_str = parts[1].trim();
            let mut lhs_parser = SKParser::new(lhs_str);
            let lhs_expr = match lhs_parser.parse_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    eprintln!("Line {:<5}: {}", line_index + 1, highlight::colorize_plain(
                        &format!("Error parsing LHS in test '{}': {}", test_line, e),
                        "red",
                        &mode
                    ));
                    continue;
                }
            };
            let mut rhs_parser = SKParser::new(rhs_str);
            let rhs_expr = match rhs_parser.parse_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    eprintln!("Line {:<5}: {}", line_index + 1, highlight::colorize_plain(
                        &format!("Error parsing RHS in test '{}': {}", test_line, e),
                        "red",
                        &mode
                    ));
                    continue;
                }
            };
            let lhs_subst = substitute_expr(&lhs_expr, &defs);
            let lhs_norm = normalize(&lhs_subst);
            let rhs_subst = substitute_expr(&rhs_expr, &defs);
            let rhs_norm = normalize(&rhs_subst);
            if lhs_norm == rhs_norm {
                println!("Line {:<5}: {}: {}",
                    line_index + 1,
                    highlight::colorize_plain("Test passed", "green", &mode),
                    to_display_expr(&lhs_norm, &defs).to_highlighted_string(&mode, 0)
                );
            } else {
                let lhs_disp = to_display_expr(&lhs_norm, &defs);
                let rhs_disp = to_display_expr(&rhs_norm, &defs);
                let (diff_lhs, diff_rhs) = diff_exprs_sym(&lhs_disp, &rhs_disp);
                println!("Line {:<5}: {}:\n    LHS: {}\n    RHS: {}",
                    line_index + 1,
                    highlight::colorize_plain("Test failed", "red", &mode),
                    lhs_disp.to_highlighted_string(&mode, 0),
                    rhs_disp.to_highlighted_string(&mode, 0)
                );
                println!("    LHS diff: {}\n    RHS diff: {}",
                    diff_lhs.to_highlighted_string(&mode, 0),
                    diff_rhs.to_highlighted_string(&mode, 0)
                );
            }
        } else if trimmed.contains('=') {
            // Process definition line.
            match parse_definition_line(trimmed) {
                Ok((name, expr)) => {
                    defs.insert(name, expr);
                },
                Err(e) => {
                    eprintln!("Line {:<5}: Definition parse error: {}", line_index + 1, e);
                    continue;
                }
            }
        } else {
            // Process final expression.
            let mut parser = SKParser::new(trimmed);
            match parser.parse_expr() {
                Ok(expr) => {
                    final_expr_opt = Some(expr);
                },
                Err(e) => {
                    eprintln!("Line {:<5}: Error parsing final expression '{}': {}", line_index + 1, trimmed, e);
                }
            }
        }
    }

    if let Some(expr) = final_expr_opt {
        println!("{}      : {}", highlight::colorize_plain("Final expression", "blue", &mode) , to_display_expr(&expr,&defs).to_highlighted_string(&mode, 0));
        let substituted_expr = substitute_expr(&expr, &defs);
        println!("{}    : {}", highlight::colorize_plain("After substitution", "blue", &mode) , to_display_expr(&substituted_expr,&defs).to_highlighted_string(&mode, 0));
        let normalized_expr = normalize(&substituted_expr);
        println!("{} : {}", highlight::colorize_plain("Normalized expression", "blue", &mode) , to_display_expr(&normalized_expr,&defs).to_highlighted_string(&mode, 0));
    }

    Ok(())
}

/// Highlighter module for syntax highlighting.
pub mod highlight {
    /// Highlight mode enum.
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum HighlightMode {
        None,
        Color16,
        Color256,
        TrueColor,
    }

    impl HighlightMode {
        pub fn from_str(s: &str) -> HighlightMode {
            match s {
                "false" => HighlightMode::None,
                "16" => HighlightMode::Color16,
                "256" => HighlightMode::Color256,
                "true" => HighlightMode::TrueColor,
                _ => HighlightMode::None,
            }
        }
    }

    /// Returns the reset escape sequence.
    pub fn reset(mode: &HighlightMode) -> String {
        match mode {
            HighlightMode::None => "".to_string(),
            _ => "\x1b[0m".to_string(),
        }
    }

    /// Color functions.
    pub mod colors {
        use super::HighlightMode;
        // use super::reset;

        pub fn pink(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[35m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;207m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;250;105;200m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn blue(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[34m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;27m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;50;50;255m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn white(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[37m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;15m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;255;255;255m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn green(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[32m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;82m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;100;230;60m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn red(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[31m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;196m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;250;80;50m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn yellow(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[33m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;11m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;240;230;0m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn orange(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[33m".to_string(), // Approximated with yellow in Color16 mode as standard ANSI does not include orange.
                HighlightMode::Color256 => "\x1b[38;5;208m".to_string(), // ANSI escape for orange in Color256 mode.
                HighlightMode::TrueColor => "\x1b[38;2;255;165;0m".to_string(), // ANSI escape for orange in TrueColor mode.
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn lightblue(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[94m".to_string(),
                HighlightMode::Color256 => "\x1b[38;5;153m".to_string(),
                HighlightMode::TrueColor => "\x1b[38;2;53;255;255m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn greenbg(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[42m".to_string(),
                HighlightMode::Color256 => "\x1b[48;5;82m".to_string(),
                HighlightMode::TrueColor => "\x1b[48;2;50;100;30m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
        pub fn redbg(mode: &HighlightMode) -> String {
            match mode {
                HighlightMode::Color16 => "\x1b[41m".to_string(),
                HighlightMode::Color256 => "\x1b[48;5;196m".to_string(),
                HighlightMode::TrueColor => "\x1b[48;2;250;80;50m".to_string(),
                HighlightMode::None => "".to_string(),
            }
        }
    }

    /// Returns an escape code for opening parentheses color based on depth.
    pub fn paren_color(depth: usize, mode: &HighlightMode) -> String {
        if *mode == HighlightMode::None {
            return "".to_string();
        }
        match mode {
            HighlightMode::Color16 => {
                let palette = [91, 92, 93, 94, 95, 96];
                let code = palette[depth % palette.len()];
                format!("\x1b[{}m", code)
            },
            HighlightMode::Color256 => {
                let palette = [196, 202, 208, 214, 220, 226];
                let code = palette[depth % palette.len()];
                format!("\x1b[38;5;{}m", code)
            },
            HighlightMode::TrueColor => {
                // For open parentheses, we use a default truecolor palette.
                let palette = [
                    (164, 219, 211), // #A4DBD3
                    (217, 201, 145), // #D9C991
                    (145, 189, 217), // #91BDD9
                    (217, 187, 145), // #D9BB91
                    (132, 137, 140), // #84898C
                ];
                let (r, g, b) = palette[depth % palette.len()];
                format!("\x1b[38;2;{};{};{}m", r, g, b)
            },
            HighlightMode::None => "".to_string(),
        }
    }

    /// Colorize a plain string with the given color (by name) for foreground.
    pub fn colorize_plain(text: &str, color: &str, mode: &HighlightMode) -> String {
        if *mode == HighlightMode::None {
            return text.to_string();
        }
        let color_code = match color {
            "pink" => colors::pink(mode),
            "blue" => colors::blue(mode),
            "white" => colors::white(mode),
            "green" => colors::green(mode),
            "red" => colors::red(mode),
            _ => "".to_string(),
        };
        format!("{}{}{}", color_code, text, reset(mode))
    }
}
