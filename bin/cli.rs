// TODO This does nothing yet! :)

use getopts;
use std::env;

fn print_usage(program: &str) {
    println!("Usage: {} [options] input.ls", program);
    println!("-h, --help\tPrint this message");
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let program = args[0].clone();

    let mut opts = getopts::Options::new();
    opts.optflag("h", "help", "displays help message");

    let matches = opts.parse(&args[1..]).expect("error parsing args");
    if matches.opt_present("h") {
        print_usage(&program);
    }
}
