# C++ Websocket Server

## Dependencies

In addition to the standard build chain including a C++ compiler with C++14 support, the following libraries are required:

* [JsonCpp](https://github.com/open-source-parsers/jsoncpp) - JSON encoding and decoding
* [tclap](http://tclap.sourceforge.net/) - command line argument parsing
* [websocketpp](https://github.com/zaphoyd/websocketpp) - websocket support
* [boost](http://www.boost.org/) - networking library

On Ubuntu 16.04 the required dependencies can be installed with the following command:

```
sudo apt install build-essential libjsoncpp-dev libtclap-dev libwebsocketpp-dev libboost-all-dev
```

## Build Instructions

After all dependencies are installed, just run `make` at the top-level project root.

```
make
```

## Run

From the project root, run the C++ server with the following command:

```
bin/cpp-websocket-server --address REPLACEME --port 3334
```
