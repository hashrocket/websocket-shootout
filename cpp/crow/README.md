# Crow C++ Websocket Server

Implementation is a modified version of the websockets example for crow:

https://github.com/ipkn/crow/blob/4e39b23e455e455f1878b3e68d729a1737f3e431/examples/websocket/example_ws.cpp

## Dependencies

In addition to the standard build chain including a C++ compiler with C++14 support, the following libraries are required:

* [RapidJSON](https://github.com/miloyip/rapidjson) - JSON encoding and decoding
* [tclap](http://tclap.sourceforge.net/) - command line argument parsing
* [crow](https://github.com/ipkn/crow) - crow framework
* [boost](http://www.boost.org/) - networking library

RapidJSON and crow are vendored with git submodules.  Be sure they are initialized before doing further.

On Ubuntu 16.04, tclap and boost can be installed with the following commands:

```
sudo apt install build-essential libtclap-dev libboost-all-dev
```

## Build Instructions

After all dependencies are installed, just run `make` at the top-level project root.

```
make
```

## Run

From the project root, run the C++ server with the following command:

```
bin/cpp-crow-server -p 3334 --thread 8
```
