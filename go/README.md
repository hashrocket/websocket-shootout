# Go

## Dependencies

* Go 1.7

All other dependencies are vendored in the project GOPATH with git submodules.

Download them with the following commands:

```
git submodule init
git submodule update
```


## Building the server

From the top-level project directory:

```
make
```

If you want to run any go commands directly you will need to set the environment as follows from top-level project root:

```
export GOPATH=`pwd`/go
export PATH=$GOPATH/bin:$PATH
```

## Websocket Server

The websocket server is located in `./src/hashrocket/go-websocket-server`.

To run the go Server

```
bin/go-websocket-server -address REPLACEME -port 3334
```
