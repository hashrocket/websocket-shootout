use serde_json;
use serde_json::Value;
use std::sync::atomic::{AtomicUsize, Ordering};
use scoped_pool as sp;
use ws;

const NULL_PAYLOAD: &'static Value = &Value::Null;

pub struct BenchHandler<'a: 'b, 'b> {
    ws: ws::Sender,
    count: &'a AtomicUsize,
    pool: &'b sp::Scope<'a>,
}

impl<'a, 'b> BenchHandler<'a, 'b> {
    pub fn run(address: &str, port: &str, threads: usize) {
        let count = AtomicUsize::new(0);
        let pool = sp::Pool::new(threads);

        pool.scoped(|scope| {
            ws::Builder::new()
                .with_settings(ws::Settings { max_connections: 500_000, ..Default::default() })
                .build(|out| {
                    BenchHandler {
                        ws: out,
                        count: &count,
                        pool: scope,
                    }
                })
                .unwrap()
                .listen(&*format!("{}:{}", address, port))
                .unwrap()
                ;
        });
    }
}

impl<'a, 'b> ws::Handler for BenchHandler<'a, 'b> {
    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        let ws = self.ws.clone();
        let count = self.count;

        self.pool.execute(move|| {
            if let Ok(Ok(Value::Object(obj))) = msg.as_text().map(serde_json::from_str::<Value>) {
                if let Some(&Value::String(ref s)) = obj.get("type") {
                    if s == "echo" {
                        ws.send(msg).unwrap();
                    } else if s == "broadcast" {
                        ws.broadcast(msg).unwrap();
                        ws.send(format!(r#"{{"type":"broadcastResult","listenCount": {},"payload":{}}}"#,
                                                  count.load(Ordering::SeqCst),
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


