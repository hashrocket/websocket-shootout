extern crate ws;
extern crate clap;
extern crate serde;
extern crate serde_json;

use serde_json::Value;
use serde_json::value::Map;


use clap::{App, Arg};

struct BenchHandler {
    ws: ws::Sender,
    count: u32
}

impl ws::Handler for BenchHandler {
    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        if let Some((Some(msg_type), payload)) = msg.into_text().ok()
            .map(|v| v.as_str().to_owned())
            .and_then(|body: String| serde_json::from_str(body.as_str()).ok())
            .and_then(|j: Value| j.as_object().map(move |obj: &Map<String, Value>| {
                let t = obj.get("type").and_then(|t| t.as_str()).map(|s| s.to_owned());
                let p: Value = obj.get("payload").unwrap_or(&Value::Null).clone();
                (t, p)
            })) {
                match msg_type.as_ref() {
                    "echo" => {
                        try!(self.ws.send(format!("{}", payload)));
                    },
                    "broadcast" => {
                        try!(self.ws.broadcast(format!("{}", payload)));
                        try!(self.ws.send(format!("{{\"type\": \"broadcastResult\", \"listenCount\": {}, \"payload\": {}}}", self.count, payload)))
                    },
                    _ => {}
                };
            };
        Ok(())
    }

    fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()> {
        self.count += 1;
        println!("Connection Open! {}", self.count);
        Ok(())
    }

    fn on_close(&mut self,_: ws::CloseCode, _: &str) {
        self.count -= 1;
        println!("Connection Closed! {}", self.count);
    }
}

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
        if let Err(error) = ws::listen(format!("{}:{}", address, port).as_str(), |out| {
            BenchHandler { ws: out, count: 0 }
        }) {
            println!("Failed to create WebSocket due to {:?}", error);
        }
    } else {
        println!("{}", matches.usage());
    }
}
