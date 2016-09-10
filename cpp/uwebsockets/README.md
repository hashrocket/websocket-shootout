# C++ uWebSockets Server

## Dependencies

In addition to the standard build chain including a C++ compiler with C++14 support, the following libraries are required:

* [RapidJSON](https://github.com/miloyip/rapidjson) - JSON encoding and decoding
* [tclap](http://tclap.sourceforge.net/) - command line argument parsing
* [uWebSockets](https://github.com/uWebSockets/uWebSockets) - websocket support

RapidJSON and uWebSockets are vendored with git submodules. Be sure these are initialized before going further.

On Ubuntu 16.04, tclap can be installed with the following command:

```
sudo apt install libtclap-dev
```

uWebSockets needs to be built. First install its dependencies:

```
sudo apt install build-essential cmake
```

Then:

```
cd vendor/uWebSockets
cmake .
make
```

There seem to be a few problems with how uWebSocket's `make install` installs itself on Ubuntu 16.04. First, it installs its `.so` to `/usr/lib64` which is not in the default `LD_LIBRARY_PATH`. Second, it doesn't copy all required headers to the system header path. So so not run `make install`. Instead the `Makefile` references the headers and shared library from the vendor directory.

## Build Instructions

After all dependencies are set up, just run `make`.

```
make
```

## Run

```
bin/cpp-uwebsockets-server --address REPLACEME --port 3334
```
