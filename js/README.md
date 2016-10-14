# Node.JS - Websocket server implementations

## Prerequisites

- Node.JS 6+

## Build

Install dependencies:

```bash
npm install
```

## Run

Run the server implementations directly:

```bash
node uws/index.js
```

Or, run the implementations in a cluster:

```bash
node run-cluster.js uws
```

By default, the cluster will spawn a worker process for each CPU (or cpu core).
To specify the number of workers to spawn, provide the desired number to
`run-cluster.js`:

```bash
node run-cluster.js uws 3
```

`run-cluster.js` will spawn 3 worker processes.
