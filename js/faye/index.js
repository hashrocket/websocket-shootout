'use strict';

var WebSocket = require('faye-websocket');
var cluster   = require('cluster');
var http      = require('http');

var server  = http.createServer();
var clients = new Set();

if (cluster.isWorker) {
  process.on('message', function(msg) {
    clients.forEach(function each(client) {
      client.send(msg);
    });
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
  clients.forEach(function each(client) {
    client.send(msg);
  });

  ws.send(JSON.stringify({type: 'broadcastResult', payload: payload}));
}

server.on('upgrade', function upgrade(request, socket, head) {
  var ws = new WebSocket(request, socket, head);

  ws.on('message', function incoming(evt) {
    var msg = JSON.parse(evt.data);
    switch (msg.type) {
      case 'echo':
        echo(ws, msg.payload);
        break;
      case 'broadcast':
        broadcast(ws, msg.payload);
        break;
      default:
        console.log('unknown message type: %s', evt.data);
    }
  });

  ws.on('close', function close() {
    clients.delete(ws);
  });

  clients.add(ws);
});

server.listen(3334);
