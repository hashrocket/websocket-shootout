# Java - Undertow websocket server implementation

## Prerequisites

- JDK 8

## Build

`../gradlew` will build and run the websocket server on port 3030 on path `/ws`

`../gradlew distTar` will create an application bundle in `build/distributions`

## Run

To run the application bundle:

```bash
tar xf build/distributions/undertow.tar
./build/distributions/undertow/bin/undertow
```

Run websocket-bench against the server

```bash
# In the root of websocket-shootout
./bin/websocket-bench broadcast ws://localhost:3030/ws
```
