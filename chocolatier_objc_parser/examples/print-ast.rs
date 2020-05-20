use chocolatier_objc_parser::ast;
use chocolatier_objc_parser::xcode::Target;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "print-ast", about = "Shows the AST parsed by chocolatier.")]
struct Opt {
    /// Also show the clang raw AST.
    #[structopt(long = "clang-raw")]
    show_clang_raw_ast: bool,

    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Target
    #[structopt(long, possible_values = &Target::variants(), default_value = Target::MacOsX86_64.triple())]
    target: Target,
}

fn main() {
    let opt = Opt::from_args();

    let content = std::fs::read_to_string(opt.input).unwrap();
    if opt.show_clang_raw_ast {
        ast::print_full_clang_ast(opt.target, &content);
    }

    let items = ast::ast_from_str(opt.target, &content).unwrap();
    println!("{:#?}", items);
}
