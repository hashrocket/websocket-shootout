use serde_json;
use serde_json::Value;
use std::sync::atomic::{AtomicUsize, Ordering};
use mioco;
use ws;

const NULL_PAYLOAD: &'static Value = &Value::Null;

pub struct BenchHandler<'a> {
    ws: ws::Sender,
    count: &'a AtomicUsize,
}

impl<'a> BenchHandler<'a> {
    pub fn run(address: &str, port: &str, threads: usize) {
        let address = String::from(address);
        let port = String::from(port);

        mioco::start_threads(threads, move|| {
            let count = AtomicUsize::new(0);

            ws::Builder::new()
                .with_settings(ws::Settings { max_connections: 500_000, ..Default::default() })
                .build(|out| {
                    BenchHandler {
                        ws: out,
                        count: &count,
                    }
                })
                .unwrap()
                .listen(&*format!("{}:{}", address, port))
                .unwrap()
                ;
        }).unwrap();
    }
}

impl<'a> ws::Handler for BenchHandler<'a> {
    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        // would love to not preload the atomic but the 'static bound
        // on the execute function demands it :(
        let ws = self.ws.clone();
        let count = self.count.load(Ordering::SeqCst);

        mioco::spawn(move|| {
            if let Ok(Ok(Value::Object(obj))) = msg.as_text().map(serde_json::from_str::<Value>) {
                if let Some(&Value::String(ref s)) = obj.get("type") {
                    if s == "echo" {
                        ws.send(msg).unwrap();
                    } else if s == "broadcast" {
                        ws.broadcast(msg).unwrap();
                        ws.send(format!(r#"{{"type":"broadcastResult","listenCount": {},"payload":{}}}"#,
                                                  count,
                                                  obj.get("payload").unwrap_or(NULL_PAYLOAD))).unwrap();
                    }
                }
            }
        });
        Ok(())
    }

    fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()> {
        self.count.fetch_add(1, Ordering::SeqCst);
        Ok(())
    }

    fn on_close(&mut self, _: ws::CloseCode, _: &str) {
        self.count.fetch_sub(1, Ordering::SeqCst);
    }
}


