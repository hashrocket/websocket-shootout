# websocket-shootout

This project is designed to compare websocket servers in multiple languages and frameworks and has a [companion blog post][post]. The servers all implement an extremely simple protocol with only two messages: `echo` and `broadcast`. An echo is returned to the sending client. A broadcast is sent to all connected clients. Both messages take a payload value that should be delivered to the appropriate destination.

[post]: https://hashrocket.com/blog/posts/websocket-shootout

Example broadcast message:

```
{"type":"broadcast","payload":{"foo": "bar"}}
```

For the platforms with low level websocket implementations the above message would work directly. For platforms with higher level abstractions such as Phoenix and Rails the message must be encoded to be compatible with their message standards.

## Platforms

The following platforms currently have servers implemented.

* Clojure
* C++
* Elixir / Phoenix
* Go
* Haskell
* Java
* Javascript / NodeJS
* Ruby / EventMachine
* Ruby / Rails
* Rust

### Build Instructions

Some dependencies are tracked via git submodules. First step is to pull them down.

```
git submodule init
git submodule update
```

Look for a README.md in each projects directory for instructions on building and running the servers.

## Benchmark

As part of this comparison a benchmark tool `websocket-bench` was built to test the performance of these websocket servers. `websocket-bench` is designed to find how many connections a server can handle while providing an acceptable level of performance. For example, given the requirement that 4 broadcast requests are served concurrently and 95% of broadcasts be completed within 500ms, how many connections can the server handle?

Here is an example benchmark run:

```
% % bin/websocket-bench broadcast ws://earth.local:3334/ws --concurrent 10 --sample-size 100 --step-size 1000 --limit-percentile 95 --limit-rtt 250ms
clients:  1000    95per-rtt:  47ms    min-rtt:   9ms    median-rtt:  20ms    max-rtt:  66ms
clients:  2000    95per-rtt:  87ms    min-rtt:   9ms    median-rtt:  43ms    max-rtt: 105ms
clients:  3000    95per-rtt: 121ms    min-rtt:  21ms    median-rtt:  58ms    max-rtt: 201ms
clients:  4000    95per-rtt: 163ms    min-rtt:  30ms    median-rtt:  76ms    max-rtt: 325ms
clients:  5000    95per-rtt: 184ms    min-rtt:  37ms    median-rtt:  95ms    max-rtt: 298ms

```

The above benchmark starts by connecting 1000 websocket clients to ws://earth.local:3334/ws. Then it sends 100 broadcast requests with a concurrency of 10. It increases by 1000 clients at a time until the 95th percentile round-trip time exceeds 250ms.

Run `make` to build the benchmark tool. Ensure you have previously initialized git submodules or the build will fail. The benchmark tool will be built to `bin/websocket-bench`.

## Open file limits

Most servers have sufficient performance to encounter OS level open file limits. Here is how to increase those limits.

### Ubuntu 16.04

Add the following to `/etc/sysctl.conf`:

```
fs.file-max = 2097152
```

Add the following to `/etc/security/limits.conf`:

```
*    soft nofile 1048576
*    hard nofile 1048576
```

In the shell run (or put in .profile or the like):

```
ulimit -n 1048576
```

## Running Benchmarks

It is *highly* recommended that `websocket-bench` and the server be run on separate machines connected with at least GB ethernet.

Run `websocket-bench` with the `--help` parameter for detailed info.

```
% bin/websocket-bench --help
```

## Outbound connection limits

A host can only establish a few ten-thousands of outbound connections before it suffer port exhaustion. To be more accurate that limit is per IP address. `websocket-bench` can use multiple IP addresses to establish more connections.

```
bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --step-size 1000
```

The above command would use addresses 192.168.50.5, 192.168.50.246, and 192.168.50.247.

Of course, this requires that the host _have_ multiple IP addresses. On Ubuntu 16.04 additional addresses can be bound to an interface by adding configuration to /etc/network/interfaces (this may require disabling network-manager if the machine is a desktop installation).

Example /etc/network/interfaces snippet:

```
...
up /sbin/ip addr add 192.168.50.246/24 dev eth0
up /sbin/ip addr add 192.168.50.247/24 dev eth0

down /sbin/ip addr del 192.168.50.246/24 dev eth0
down /sbin/ip addr del 192.168.50.247/24 dev eth0
...
```

## Results

Results are in the results directory.

## Contributing

This project is complete and is no longer actively maintained. We'll leave pull
requests demonstrating other implementations open for educational purposes.

## About

[![Hashrocket logo](https://hashrocket.com/hashrocket_logo.svg)](https://hashrocket.com)

websocket-shootout is supported by the team at [Hashrocket, a multidisciplinary design and development consultancy](https://hashrocket.com). If you'd like to [work with us](https://hashrocket.com/contact-us/hire-us) or [join our team](https://hashrocket.com/contact-us/jobs), don't hesitate to get in touch.
