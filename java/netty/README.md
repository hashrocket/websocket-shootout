# Java - Netty websocket server implementation

This implementation is a slightly modified version of the [`benchmarkserver`
Netty example here](https://github.com/netty/netty/tree/netty-4.1.5.Final/example/src/main/java/io/netty/example/http/websocketx/benchmarkserver)

## Prerequisites

- JDK 8

## Build

`../gradlew` will build and run the websocket server on port 3031 on path `/ws`

`../gradlew distTar` will create an application bundle in `build/distributions`

## Run

To run the application bundle:

```bash
tar xf build/distributions/netty.tar
./build/distributions/netty/bin/netty
```

Run websocket-bench against the server

```bash
# In the root of websocket-shootout
./bin/websocket-bench broadcast ws://localhost:3031/ws
```
