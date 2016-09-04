extern crate ws;
extern crate clap;
extern crate serde;
extern crate serde_json;

use clap::{App, Arg};

mod single_threaded_ws;
use single_threaded_ws::BenchHandler;

fn main() {
    let matches = App::new("rust-ws-server")
                      .version("1.0")
                      .arg(Arg::with_name("address")
                               .short("a")
                               .long("address")
                               .required(true)
                               .takes_value(true)
                               .default_value("127.0.0.1"))
                      .arg(Arg::with_name("port")
                               .short("p")
                               .long("port")
                               .required(true)
                               .takes_value(true)
                               .default_value("3000"))
                      .get_matches();

    if let (Some(address), Some(port)) = (matches.value_of("address"), matches.value_of("port")) {
        BenchHandler::run(address, port);
    } else {
        println!("{}", matches.usage());
    }
}
