use serde_json;
use serde_json::Value;
use ws;
use std::cell::Cell;

const NULL_PAYLOAD: &'static Value = &Value::Null;

pub struct BenchHandler<'a> {
    ws: ws::Sender,
    count: &'a Cell<usize>,
}

impl<'a> BenchHandler<'a> {
    pub fn run(address: &str, port: &str) {
        let conns = Cell::new(0);

        ws::Builder::new()
            .with_settings(ws::Settings { max_connections: 500_000, ..Default::default() })
            .build(|out| {
                BenchHandler {
                    ws: out,
                    count: &conns,
                }
            })
        .unwrap()
        .listen(&*format!("{}:{}", address, port))
        .unwrap()
        ;
    }
}

impl<'a> ws::Handler for BenchHandler<'a> {
    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        if let Ok(Ok(Value::Object(obj))) = msg.as_text().map(serde_json::from_str::<Value>) {
            if let Some(&Value::String(ref s)) = obj.get("type") {
                if s == "echo" {
                    try!(self.ws.send(msg))
                } else if s == "broadcast" {
                    try!(self.ws.broadcast(msg));
                    try!(self.ws.send(format!(r#"{{"type":"broadcastResult","listenCount": {},"payload":{}}}"#,
                                              self.count.get(),
                                              obj.get("payload").unwrap_or(NULL_PAYLOAD))))
                }
            }
        }
        Ok(())
    }

    fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()> {
        self.count.set(self.count.get() + 1);
        Ok(())
    }

    fn on_close(&mut self, _: ws::CloseCode, _: &str) {
        self.count.set(self.count.get() - 1);
    }
}


