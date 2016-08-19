var WebSocketServer = require('ws').Server;
var wss             = new WebSocketServer({ port: 3334 });

function echo(ws, payload) {
  ws.send(JSON.stringify({type: "echo", payload: payload}));
}

function broadcast(ws, payload) {
  var msg = JSON.stringify({type: "broadcast", payload: payload});
  wss.clients.forEach(function each(client) {
    client.send(msg);
  });

  ws.send(JSON.stringify({type: "broadcastResult", payload: payload}));
}

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(message) {
    var msg = JSON.parse(message);
    switch (msg.type) {
      case "echo":
        echo(ws, msg.payload);
        break;
      case "broadcast":
        broadcast(ws, msg.payload);
        break;
      default:
        console.log("unknown message type: %s", message);
    }
  });
});
