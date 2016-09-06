extern crate clap;
extern crate mioco;
extern crate num_cpus;
extern crate scoped_pool;
extern crate serde;
extern crate serde_json;
extern crate threadpool;
extern crate ws;

use clap::{App, Arg, ArgGroup};

mod single_threaded_ws;
mod mioco_ws;
mod threadpool_ws;
mod scopedpool_ws;

fn main() {
    // idea is 2 threads per core, but leave 1 for handling the ws connection itself
    let default_threads = 2 * num_cpus::get() - 1;

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
                      .arg(Arg::with_name("ws"))
                      .arg(Arg::with_name("threadpool-ws"))
                      .arg(Arg::with_name("scopedpool-ws"))
                      .arg(Arg::with_name("mioco-ws"))
                      .arg(Arg::with_name("threads")
                            .short("t"))
                      .group(ArgGroup::with_name("impls")
                           .args(&["ws", "threadpool-ws", "scopedpool-ws", "mioco-ws"])
                           .required(true)
                           )
                     .get_matches();

    if let (Some(address), Some(port)) = (matches.value_of("address"), matches.value_of("port")) {
        let implementation = matches.value_of("impls").unwrap();
        
        match implementation {
            "ws" => single_threaded_ws::BenchHandler::run(address, port),
            "threadpool-ws" => {
                let threads: usize = matches.value_of("threads").and_then(|t| t.parse().ok()).unwrap_or(default_threads);
                threadpool_ws::BenchHandler::run(address, port, threads);
            },
            "scopedpool-ws" => {
                let threads: usize = matches.value_of("threads").and_then(|t| t.parse().ok()).unwrap_or(default_threads);
                scopedpool_ws::BenchHandler::run(address, port, threads);
            },
            "mioco-ws" => {
                let threads: usize = matches.value_of("threads").and_then(|t| t.parse().ok()).unwrap_or(default_threads);
                mioco_ws::BenchHandler::run(address, port, threads);
            },
            _ => unreachable!{}
        }
    } else {
        println!("{}", matches.usage());
    }
}
