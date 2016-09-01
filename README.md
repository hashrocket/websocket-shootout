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
* Javascript / NodeJS
* Ruby / Rails

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

These results are from running the server on one machine and the benchmark tool as another. Both machines are bare metal 4ghz i7 4790Ks with 16GB of RAM running Ubuntu 16.04 connected via GB ethernet. Tests were run multiple times and the best results were recorded.

C++
```
$ bin/websocket-bench broadcast ws://earth.local:3334/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000
clients:  1000    95per-rtt:  41ms    min-rtt:   7ms    median-rtt:  12ms    max-rtt:  44ms
clients:  2000    95per-rtt:  42ms    min-rtt:  14ms    median-rtt:  26ms    max-rtt:  47ms
clients:  3000    95per-rtt:  50ms    min-rtt:  26ms    median-rtt:  34ms    max-rtt:  52ms
clients:  4000    95per-rtt:  57ms    min-rtt:  36ms    median-rtt:  48ms    max-rtt:  76ms
clients:  5000    95per-rtt:  90ms    min-rtt:  37ms    median-rtt:  51ms    max-rtt:  97ms
clients:  6000    95per-rtt:  78ms    min-rtt:  54ms    median-rtt:  64ms    max-rtt:  87ms
clients:  7000    95per-rtt: 134ms    min-rtt:  57ms    median-rtt:  87ms    max-rtt: 141ms
clients:  8000    95per-rtt: 123ms    min-rtt:  84ms    median-rtt:  97ms    max-rtt: 132ms
clients:  9000    95per-rtt: 180ms    min-rtt:  78ms    median-rtt: 100ms    max-rtt: 187ms
clients: 10000    95per-rtt: 160ms    min-rtt:  78ms    median-rtt: 113ms    max-rtt: 180ms
clients: 11000    95per-rtt: 197ms    min-rtt:  94ms    median-rtt: 126ms    max-rtt: 252ms
clients: 12000    95per-rtt: 195ms    min-rtt:  92ms    median-rtt: 136ms    max-rtt: 197ms
clients: 13000    95per-rtt: 188ms    min-rtt: 123ms    median-rtt: 152ms    max-rtt: 199ms
clients: 14000    95per-rtt: 187ms    min-rtt: 125ms    median-rtt: 144ms    max-rtt: 244ms
clients: 15000    95per-rtt: 240ms    min-rtt: 123ms    median-rtt: 158ms    max-rtt: 258ms
clients: 16000    95per-rtt: 411ms    min-rtt: 125ms    median-rtt: 191ms    max-rtt: 424ms
clients: 17000    95per-rtt: 375ms    min-rtt: 150ms    median-rtt: 198ms    max-rtt: 375ms
clients: 18000    95per-rtt: 328ms    min-rtt: 167ms    median-rtt: 240ms    max-rtt: 330ms
clients: 19000    95per-rtt: 272ms    min-rtt: 150ms    median-rtt: 211ms    max-rtt: 377ms
clients: 20000    95per-rtt: 254ms    min-rtt: 174ms    median-rtt: 213ms    max-rtt: 373ms
clients: 21000    95per-rtt: 364ms    min-rtt: 161ms    median-rtt: 276ms    max-rtt: 468ms
clients: 22000    95per-rtt: 315ms    min-rtt: 220ms    median-rtt: 251ms    max-rtt: 317ms
clients: 23000    95per-rtt: 263ms    min-rtt: 189ms    median-rtt: 230ms    max-rtt: 372ms
clients: 24000    95per-rtt: 348ms    min-rtt: 203ms    median-rtt: 263ms    max-rtt: 373ms
clients: 25000    95per-rtt: 341ms    min-rtt: 267ms    median-rtt: 295ms    max-rtt: 359ms
clients: 26000    95per-rtt: 358ms    min-rtt: 234ms    median-rtt: 283ms    max-rtt: 581ms
clients: 27000    95per-rtt: 412ms    min-rtt: 215ms    median-rtt: 278ms    max-rtt: 466ms
clients: 28000    95per-rtt: 444ms    min-rtt: 274ms    median-rtt: 347ms    max-rtt: 448ms
clients: 29000    95per-rtt: 383ms    min-rtt: 270ms    median-rtt: 322ms    max-rtt: 450ms
clients: 30000    95per-rtt: 473ms    min-rtt: 253ms    median-rtt: 328ms    max-rtt: 562ms
clients: 31000    95per-rtt: 471ms    min-rtt: 293ms    median-rtt: 381ms    max-rtt: 517ms
clients: 32000    95per-rtt: 463ms    min-rtt: 290ms    median-rtt: 369ms    max-rtt: 483ms
clients: 33000    95per-rtt: 476ms    min-rtt: 299ms    median-rtt: 352ms    max-rtt: 692ms
```

Clojure
```
$ bin/websocket-bench broadcast ws://earth.local:3334/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000
clients:  1000    95per-rtt:  48ms    min-rtt:   7ms    median-rtt:  40ms    max-rtt:  49ms
clients:  2000    95per-rtt:  57ms    min-rtt:  22ms    median-rtt:  43ms    max-rtt:  57ms
clients:  3000    95per-rtt:  61ms    min-rtt:  16ms    median-rtt:  47ms    max-rtt:  68ms
clients:  4000    95per-rtt:  85ms    min-rtt:  18ms    median-rtt:  52ms    max-rtt:  95ms
clients:  5000    95per-rtt:  92ms    min-rtt:  24ms    median-rtt:  58ms    max-rtt: 107ms
clients:  6000    95per-rtt: 121ms    min-rtt:  30ms    median-rtt:  62ms    max-rtt: 157ms
clients:  7000    95per-rtt: 189ms    min-rtt:  34ms    median-rtt:  66ms    max-rtt: 194ms
clients:  8000    95per-rtt: 161ms    min-rtt:  43ms    median-rtt:  81ms    max-rtt: 236ms
clients:  9000    95per-rtt: 168ms    min-rtt:  47ms    median-rtt:  86ms    max-rtt: 183ms
clients: 10000    95per-rtt: 170ms    min-rtt:  46ms    median-rtt:  97ms    max-rtt: 185ms
clients: 11000    95per-rtt: 179ms    min-rtt:  56ms    median-rtt: 103ms    max-rtt: 182ms
clients: 12000    95per-rtt: 169ms    min-rtt:  62ms    median-rtt: 118ms    max-rtt: 190ms
clients: 13000    95per-rtt: 255ms    min-rtt:  56ms    median-rtt: 124ms    max-rtt: 268ms
clients: 14000    95per-rtt: 277ms    min-rtt:  66ms    median-rtt: 136ms    max-rtt: 279ms
clients: 15000    95per-rtt: 323ms    min-rtt:  71ms    median-rtt: 142ms    max-rtt: 356ms
clients: 16000    95per-rtt: 376ms    min-rtt:  68ms    median-rtt: 158ms    max-rtt: 380ms
clients: 17000    95per-rtt: 305ms    min-rtt:  81ms    median-rtt: 158ms    max-rtt: 312ms
clients: 18000    95per-rtt: 345ms    min-rtt:  80ms    median-rtt: 161ms    max-rtt: 392ms
clients: 19000    95per-rtt: 300ms    min-rtt:  88ms    median-rtt: 187ms    max-rtt: 323ms
clients: 20000    95per-rtt: 359ms    min-rtt:  98ms    median-rtt: 182ms    max-rtt: 449ms
clients: 21000    95per-rtt: 409ms    min-rtt: 102ms    median-rtt: 203ms    max-rtt: 444ms
clients: 22000    95per-rtt: 391ms    min-rtt: 113ms    median-rtt: 215ms    max-rtt: 432ms
clients: 23000    95per-rtt: 407ms    min-rtt: 104ms    median-rtt: 220ms    max-rtt: 483ms
clients: 24000    95per-rtt: 391ms    min-rtt: 112ms    median-rtt: 236ms    max-rtt: 434ms
clients: 25000    95per-rtt: 476ms    min-rtt: 111ms    median-rtt: 228ms    max-rtt: 492ms
clients: 26000    95per-rtt: 476ms    min-rtt: 114ms    median-rtt: 259ms    max-rtt: 539ms
clients: 27000    95per-rtt: 425ms    min-rtt: 161ms    median-rtt: 237ms    max-rtt: 598ms
```

Elixir / Phoenix
```
$ bin/websocket-bench broadcast ws://earth.local:4000/socket/websocket -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000 --server-type phoenix
clients:  1000    95per-rtt:  30ms    min-rtt:   5ms    median-rtt:  13ms    max-rtt:  31ms
clients:  2000    95per-rtt:  63ms    min-rtt:  12ms    median-rtt:  25ms    max-rtt: 209ms
clients:  3000    95per-rtt:  99ms    min-rtt:  13ms    median-rtt:  41ms    max-rtt: 102ms
clients:  4000    95per-rtt: 139ms    min-rtt:  19ms    median-rtt:  47ms    max-rtt: 226ms
clients:  5000    95per-rtt: 111ms    min-rtt:  22ms    median-rtt:  57ms    max-rtt: 125ms
clients:  6000    95per-rtt: 262ms    min-rtt:  30ms    median-rtt:  77ms    max-rtt: 277ms
clients:  7000    95per-rtt: 158ms    min-rtt:  28ms    median-rtt:  81ms    max-rtt: 224ms
clients:  8000    95per-rtt: 202ms    min-rtt:  36ms    median-rtt:  92ms    max-rtt: 244ms
clients:  9000    95per-rtt: 239ms    min-rtt:  31ms    median-rtt: 119ms    max-rtt: 258ms
clients: 10000    95per-rtt: 235ms    min-rtt:  65ms    median-rtt: 121ms    max-rtt: 254ms
clients: 11000    95per-rtt: 303ms    min-rtt:  46ms    median-rtt: 155ms    max-rtt: 357ms
clients: 12000    95per-rtt: 309ms    min-rtt:  65ms    median-rtt: 158ms    max-rtt: 324ms
clients: 13000    95per-rtt: 292ms    min-rtt:  50ms    median-rtt: 155ms    max-rtt: 416ms
clients: 14000    95per-rtt: 323ms    min-rtt:  78ms    median-rtt: 178ms    max-rtt: 334ms
clients: 15000    95per-rtt: 317ms    min-rtt:  55ms    median-rtt: 193ms    max-rtt: 454ms
clients: 16000    95per-rtt: 312ms    min-rtt:  72ms    median-rtt: 198ms    max-rtt: 321ms
clients: 17000    95per-rtt: 408ms    min-rtt:  65ms    median-rtt: 203ms    max-rtt: 410ms
clients: 18000    95per-rtt: 348ms    min-rtt:  75ms    median-rtt: 231ms    max-rtt: 469ms
clients: 19000    95per-rtt: 380ms    min-rtt: 108ms    median-rtt: 231ms    max-rtt: 482ms
clients: 20000    95per-rtt: 441ms    min-rtt:  90ms    median-rtt: 236ms    max-rtt: 446ms
clients: 21000    95per-rtt: 491ms    min-rtt:  85ms    median-rtt: 255ms    max-rtt: 519ms
clients: 22000    95per-rtt: 452ms    min-rtt:  85ms    median-rtt: 265ms    max-rtt: 492ms
clients: 23000    95per-rtt: 496ms    min-rtt:  71ms    median-rtt: 268ms    max-rtt: 521ms
clients: 24000    95per-rtt: 491ms    min-rtt:  93ms    median-rtt: 309ms    max-rtt: 510ms
```

Go
```
$ bin/websocket-bench broadcast ws://earth.local:3334/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000
clients:  1000    95per-rtt:  24ms    min-rtt:   4ms    median-rtt:  13ms    max-rtt:  32ms
clients:  2000    95per-rtt:  51ms    min-rtt:   9ms    median-rtt:  23ms    max-rtt: 230ms
clients:  3000    95per-rtt:  73ms    min-rtt:  16ms    median-rtt:  35ms    max-rtt:  78ms
clients:  4000    95per-rtt:  99ms    min-rtt:  18ms    median-rtt:  46ms    max-rtt: 113ms
clients:  5000    95per-rtt: 117ms    min-rtt:  26ms    median-rtt:  60ms    max-rtt: 230ms
clients:  6000    95per-rtt: 116ms    min-rtt:  30ms    median-rtt:  64ms    max-rtt: 123ms
clients:  7000    95per-rtt: 142ms    min-rtt:  38ms    median-rtt:  78ms    max-rtt: 159ms
clients:  8000    95per-rtt: 159ms    min-rtt:  40ms    median-rtt:  86ms    max-rtt: 184ms
clients:  9000    95per-rtt: 240ms    min-rtt:  41ms    median-rtt: 102ms    max-rtt: 243ms
clients: 10000    95per-rtt: 244ms    min-rtt:  46ms    median-rtt: 105ms    max-rtt: 303ms
clients: 11000    95per-rtt: 247ms    min-rtt:  55ms    median-rtt: 123ms    max-rtt: 264ms
clients: 12000    95per-rtt: 237ms    min-rtt:  57ms    median-rtt: 122ms    max-rtt: 268ms
clients: 13000    95per-rtt: 267ms    min-rtt:  66ms    median-rtt: 128ms    max-rtt: 301ms
clients: 14000    95per-rtt: 314ms    min-rtt:  71ms    median-rtt: 141ms    max-rtt: 324ms
clients: 15000    95per-rtt: 314ms    min-rtt:  82ms    median-rtt: 150ms    max-rtt: 333ms
clients: 16000    95per-rtt: 326ms    min-rtt:  76ms    median-rtt: 169ms    max-rtt: 395ms
clients: 17000    95per-rtt: 350ms    min-rtt:  86ms    median-rtt: 184ms    max-rtt: 366ms
clients: 18000    95per-rtt: 442ms    min-rtt:  79ms    median-rtt: 178ms    max-rtt: 500ms
clients: 19000    95per-rtt: 316ms    min-rtt:  93ms    median-rtt: 197ms    max-rtt: 418ms
clients: 20000    95per-rtt: 394ms    min-rtt: 101ms    median-rtt: 208ms    max-rtt: 439ms
clients: 21000    95per-rtt: 452ms    min-rtt:  95ms    median-rtt: 228ms    max-rtt: 557ms
clients: 22000    95per-rtt: 413ms    min-rtt: 100ms    median-rtt: 220ms    max-rtt: 476ms
clients: 23000    95per-rtt: 327ms    min-rtt: 123ms    median-rtt: 239ms    max-rtt: 370ms
clients: 24000    95per-rtt: 460ms    min-rtt: 112ms    median-rtt: 245ms    max-rtt: 525ms
```

Ruby MRI / Rails
```
$ bin/websocket-bench broadcast ws://earth.local:3334/cable -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 100 --origin http://earth.local/ --server-type actioncable
clients:   100    95per-rtt:  94ms    min-rtt:  42ms    median-rtt:  73ms    max-rtt: 101ms
clients:   200    95per-rtt: 184ms    min-rtt: 100ms    median-rtt: 157ms    max-rtt: 186ms
clients:   300    95per-rtt: 287ms    min-rtt: 114ms    median-rtt: 221ms    max-rtt: 287ms
clients:   400    95per-rtt: 449ms    min-rtt: 164ms    median-rtt: 310ms    max-rtt: 449ms
clients:   500    95per-rtt: 445ms    min-rtt: 132ms    median-rtt: 382ms    max-rtt: 520ms
```

Ruby JRuby 9000 / Rails
```
$ bin/websocket-bench broadcast ws://earth.local:3334/cable -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 100 --origin http://earth.local/ --server-type actioncable
clients:   100    95per-rtt:  48ms    min-rtt:  12ms    median-rtt:  41ms    max-rtt:  50ms
clients:   200    95per-rtt:  94ms    min-rtt:  32ms    median-rtt:  86ms    max-rtt:  95ms
clients:   300    95per-rtt: 151ms    min-rtt:  39ms    median-rtt: 132ms    max-rtt: 152ms
clients:   400    95per-rtt: 170ms    min-rtt:  42ms    median-rtt: 160ms    max-rtt: 170ms
clients:   500    95per-rtt: 223ms    min-rtt:  60ms    median-rtt: 199ms    max-rtt: 225ms
clients:   600    95per-rtt: 271ms    min-rtt:  66ms    median-rtt: 250ms    max-rtt: 272ms
clients:   700    95per-rtt: 298ms    min-rtt:  82ms    median-rtt: 278ms    max-rtt: 301ms
clients:   800    95per-rtt: 375ms    min-rtt:  86ms    median-rtt: 316ms    max-rtt: 382ms
clients:   900    95per-rtt: 380ms    min-rtt:  98ms    median-rtt: 349ms    max-rtt: 385ms
clients:  1000    95per-rtt: 435ms    min-rtt: 122ms    median-rtt: 397ms    max-rtt: 441ms
clients:  1100    95per-rtt: 469ms    min-rtt: 112ms    median-rtt: 442ms    max-rtt: 472ms


NodeJS / websocket/ws
```
$ bin/websocket-bench broadcast ws://earth.local:3334/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000
clients:  1000    95per-rtt:  73ms    min-rtt:  11ms    median-rtt:  35ms    max-rtt:  77ms
clients:  2000    95per-rtt: 105ms    min-rtt:  22ms    median-rtt:  72ms    max-rtt: 106ms
clients:  3000    95per-rtt: 147ms    min-rtt:  27ms    median-rtt: 107ms    max-rtt: 150ms
clients:  4000    95per-rtt: 157ms    min-rtt:  48ms    median-rtt: 108ms    max-rtt: 162ms
clients:  5000    95per-rtt: 192ms    min-rtt:  44ms    median-rtt: 179ms    max-rtt: 198ms
clients:  6000    95per-rtt: 276ms    min-rtt:  84ms    median-rtt: 216ms    max-rtt: 277ms
clients:  7000    95per-rtt: 247ms    min-rtt:  68ms    median-rtt: 230ms    max-rtt: 259ms
clients:  8000    95per-rtt: 314ms    min-rtt:  78ms    median-rtt: 283ms    max-rtt: 318ms
clients:  9000    95per-rtt: 327ms    min-rtt:  83ms    median-rtt: 297ms    max-rtt: 343ms
clients: 10000    95per-rtt: 361ms    min-rtt:  94ms    median-rtt: 351ms    max-rtt: 397ms
clients: 11000    95per-rtt: 465ms    min-rtt: 115ms    median-rtt: 300ms    max-rtt: 492ms
clients: 12000    95per-rtt: 429ms    min-rtt: 113ms    median-rtt: 405ms    max-rtt: 471ms
clients: 13000    95per-rtt: 476ms    min-rtt: 129ms    median-rtt: 433ms    max-rtt: 505ms
```

### Memory Usage

Approximate memory usage as eyeballed in htop.

* Go           ~800MB
* C++          ~600MB
* Clojure     ~1500MB
* Rails/MRI    ~150MB
* Rails/JRuby  ~650MB
* Elixir      ~1900MB
* Node/ws      ~300MB
