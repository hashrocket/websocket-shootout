'use strict';

const impl = process.argv[2];
const numProcs = +process.argv[3] ?
  +process.argv[3] : require('os').cpus().length;

if (!impl) {
  console.error('No implementation provided');
  process.exit(1);
}

switch (impl) {
  case 'ws':
  case 'uws':
  case 'faye':
    break;
  default:
    console.error(`Implementation: ${impl} not valid`);
    process.exit(1);
}

const cluster = require('cluster');

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numProcs; i++) {
    cluster.fork();
  }

  let onExit = () => {
    cluster.removeAllListeners('exit');
    Object.keys(cluster.workers).forEach((id) => {
      cluster.workers[id].kill();
    });
  };

  process
  .on('SIGINT', onExit)
  .on('SIGTERM', onExit);

  cluster.on('exit', (worker, code, signal) => {
    console.log(`worker ${worker.process.pid} died`);
    setTimeout(() => cluster.fork(), 1000);
  });

  cluster.on('message', (worker, msg) => {
    Object.keys(cluster.workers).forEach((id) => {
      if (+id !== worker.id) {
        cluster.workers[id].send(msg);
      }
    });
  });
} else {
  require(`./${impl}/`);
}
