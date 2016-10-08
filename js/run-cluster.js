'use strict';

const impl = process.argv[2];

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
const numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
  const onMessage = function (msg) {
    Object.keys(cluster.workers).forEach((id) => {
      if (+id !== this.id) {
        cluster.workers[id].send(msg);
      }
    });
  };

  cluster.on('fork', (worker) => worker.on('message', onMessage));

  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  const onExit = () => {
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
} else {
  require(`./${impl}/`);
}
