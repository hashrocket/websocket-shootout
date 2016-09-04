use serde_json;
use serde_json::Value;
use ws;

const NULL_PAYLOAD: &'static Value = &Value::Null;

pub struct BenchHandler {
    ws: ws::Sender,
    count: u32,
}

impl BenchHandler {
    pub fn run(address: &str, port: &str) {
        ws::Builder::new()
            .with_settings(ws::Settings { max_connections: 500_000, ..Default::default() })
            .build(|out| {
                BenchHandler {
                    ws: out,
                    count: 0,
                }
            })
        .unwrap()
        .listen(&*format!("{}:{}", address, port))
        .unwrap()
        ;
    }
}

impl ws::Handler for BenchHandler {
    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        if let Ok(Ok(Value::Object(obj))) = msg.as_text().map(serde_json::from_str::<Value>) {
            if let Some(&Value::String(ref s)) = obj.get("type") {
                if s == "echo" {
                    try!(self.ws.send(msg))
                } else if s == "broadcast" {
                    try!(self.ws.broadcast(msg));
                    try!(self.ws.send(format!(r#"{{"type":"broadcastResult","listenCount": {},"payload":{}}}"#,
                                              self.count,
                                              obj.get("payload").unwrap_or(NULL_PAYLOAD))))
                }
            }
        }
        Ok(())
    }

    fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()> {
        self.count += 1;
        Ok(())
    }

    fn on_close(&mut self, _: ws::CloseCode, _: &str) {
        self.count -= 1;
    }
}


