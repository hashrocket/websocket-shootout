'use strict';

var cluster = require('cluster');
var ws      = require('ws');

var wss = new ws.Server({
  perMessageDeflate: false,
  port: 3334
});

if (cluster.isWorker) {
  process.on('message', broadcastMessage);
}

function broadcastMessage(msg) {
  var buf = Buffer.from(msg);
  var opts = {binary: false};

  wss.clients.forEach(function each(ws) {
    ws.send(buf, opts);
  });
}

function echo(ws, payload) {
  ws.send(JSON.stringify({type: 'echo', payload: payload}));
}

function broadcast(ws, payload) {
  var msg = JSON.stringify({type: 'broadcast', payload: payload});

  if (cluster.isWorker) {
    process.send(msg);
  }
  broadcastMessage(msg);

  ws.send(JSON.stringify({type: 'broadcastResult', payload: payload}));
}

function onMessage(message) {
  var msg = JSON.parse(message);
  switch (msg.type) {
    case 'echo':
      echo(this, msg.payload);
      break;
    case 'broadcast':
      broadcast(this, msg.payload);
      break;
    default:
      console.log('unknown message type: %s', message);
  }
}

wss.on('connection', function connection(ws) {
  // uws removes the `upgradeReq` object right after emitting the `connection`
  // event. The same is also done here for parity.
  ws.upgradeReq = null;
  ws.on('message', onMessage);
});
