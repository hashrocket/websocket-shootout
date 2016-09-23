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

`run-cluster.js` will spawn a process for each cpu core.
