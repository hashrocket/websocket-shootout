# Round 2 Binary

Some concerns were raised about the impact JSON encoding and parsing may have on the benchmark reliability. To test this possibility the Go (golang.org/x/net/websocket), C++ (uWebsockets), and C++ (websocketpp) servers have been updated to support a binary mode.

In addition, it was suggested that using a Go program and using a complete websocket client (even when the benchmark tool is parallelized on multiple machines)  would impact the results. So as a further test, a C++ benchmark tool that directly works with libuv instead of any websocket client library has been added that supports the binary broadcast subset of the benchmark protocol. The C++ benchmark is not parallelized.

Finally, the test now starts at 10,000 clients instead of 0. This allows the tests to run a little faster. In all other ways, this round of testing should be the same as the original Round 2.

## Environments

Go

```
dev@earth:~/hashrocket/websocket-shootout(master*)% go version
go version go1.7.3 linux/amd64
```

C++

```
dev@earth:~/hashrocket/websocket-shootout/cpp/uwebsockets(master*)% gcc -v
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)
```

## Raw Go bench / JSON Results

### C++ / uWebSockets / RapidJSON -- 1 thread

Server Command

```
dev@earth:~/hashrocket/websocket-shootout/cpp/uwebsockets(master*)% bin/cpp-uwebsockets-server --port 5000 --data-type json
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+*)% bin/websocket-bench broadcast ws://192.168.50.12:5000/ \
  -l 192.168.50.5 \
  -l 192.168.50.246 \
  -l 192.168.50.247 \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --server-type json \
  -w neptune:4001 \
  -w uranus:4000 \
  -w saturn:4002 \
  -w venus:4000
clients: 10000    95per-rtt: 160ms    min-rtt:  31ms    median-rtt: 147ms    max-rtt: 290ms
clients: 11000    95per-rtt: 174ms    min-rtt:  48ms    median-rtt: 163ms    max-rtt: 180ms
clients: 12000    95per-rtt: 201ms    min-rtt:  39ms    median-rtt: 175ms    max-rtt: 333ms
clients: 13000    95per-rtt: 205ms    min-rtt:  50ms    median-rtt: 192ms    max-rtt: 212ms
clients: 14000    95per-rtt: 340ms    min-rtt:  34ms    median-rtt: 129ms    max-rtt: 1123ms
clients: 15000    95per-rtt: 236ms    min-rtt:  59ms    median-rtt: 222ms    max-rtt: 244ms
clients: 16000    95per-rtt: 261ms    min-rtt:  33ms    median-rtt: 233ms    max-rtt: 315ms
clients: 17000    95per-rtt: 269ms    min-rtt:  77ms    median-rtt: 252ms    max-rtt: 291ms
clients: 18000    95per-rtt: 297ms    min-rtt:  46ms    median-rtt: 265ms    max-rtt: 647ms
clients: 19000    95per-rtt: 298ms    min-rtt:  75ms    median-rtt: 282ms    max-rtt: 314ms
clients: 20000    95per-rtt: 322ms    min-rtt:  78ms    median-rtt: 297ms    max-rtt: 340ms
clients: 21000    95per-rtt: 423ms    min-rtt:  55ms    median-rtt: 309ms    max-rtt: 644ms
clients: 22000    95per-rtt: 335ms    min-rtt:  86ms    median-rtt: 326ms    max-rtt: 346ms
clients: 23000    95per-rtt: 417ms    min-rtt:  54ms    median-rtt: 308ms    max-rtt: 921ms
clients: 24000    95per-rtt: 371ms    min-rtt: 173ms    median-rtt: 356ms    max-rtt: 385ms
clients: 25000    95per-rtt: 411ms    min-rtt:  74ms    median-rtt: 368ms    max-rtt: 737ms
clients: 26000    95per-rtt: 408ms    min-rtt: 141ms    median-rtt: 385ms    max-rtt: 481ms
```


### C++ / WebsocketPP / jsoncpp -- 8 thread

Server Command

```
dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% bin/cpp-websocketpp-server --thread 8 --address 0.0.0.0 --port 5000 --data-type json

```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+*)% bin/websocket-bench broadcast ws://192.168.50.12:5000/ \
  -l 192.168.50.5 \
  -l 192.168.50.246 \
  -l 192.168.50.247 \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --server-type json \
  -w neptune:4001 \
  -w uranus:4000 \
  -w saturn:4002 \
  -w venus:4000
clients: 10000    95per-rtt: 278ms    min-rtt:  42ms    median-rtt: 105ms    max-rtt: 408ms
clients: 11000    95per-rtt: 315ms    min-rtt:  44ms    median-rtt: 121ms    max-rtt: 610ms
clients: 12000    95per-rtt: 299ms    min-rtt:  49ms    median-rtt: 102ms    max-rtt: 585ms
clients: 13000    95per-rtt: 326ms    min-rtt:  51ms    median-rtt: 136ms    max-rtt: 440ms
clients: 14000    95per-rtt: 406ms    min-rtt:  57ms    median-rtt: 119ms    max-rtt: 620ms
clients: 15000    95per-rtt: 363ms    min-rtt:  58ms    median-rtt: 139ms    max-rtt: 648ms
clients: 16000    95per-rtt: 483ms    min-rtt:  63ms    median-rtt: 165ms    max-rtt: 665ms
clients: 17000    95per-rtt: 432ms    min-rtt:  69ms    median-rtt: 153ms    max-rtt: 677ms
clients: 18000    95per-rtt: 473ms    min-rtt:  70ms    median-rtt: 157ms    max-rtt: 746ms
```


### Go / Websocket

Server Command

```
dev@earth:~/hashrocket/websocket-shootout(master*)% bin/go-websocket-server -address 0.0.0.0 -port 5000
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+*)% bin/websocket-bench broadcast ws://192.168.50.12:5000/json \
  -l 192.168.50.5 \
  -l 192.168.50.246 \
  -l 192.168.50.247 \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --server-type json \
  -w neptune:4001 \
  -w uranus:4000 \
  -w saturn:4002 \
  -w venus:4000
clients: 10000    95per-rtt: 259ms    min-rtt:  37ms    median-rtt:  85ms    max-rtt: 293ms
clients: 11000    95per-rtt: 283ms    min-rtt:  38ms    median-rtt:  92ms    max-rtt: 510ms
clients: 12000    95per-rtt: 279ms    min-rtt:  42ms    median-rtt: 102ms    max-rtt: 442ms
clients: 13000    95per-rtt: 333ms    min-rtt:  46ms    median-rtt: 109ms    max-rtt: 466ms
clients: 14000    95per-rtt: 427ms    min-rtt:  49ms    median-rtt: 111ms    max-rtt: 547ms
clients: 15000    95per-rtt: 406ms    min-rtt:  52ms    median-rtt: 124ms    max-rtt: 689ms
clients: 16000    95per-rtt: 364ms    min-rtt:  57ms    median-rtt: 140ms    max-rtt: 479ms
clients: 17000    95per-rtt: 336ms    min-rtt:  59ms    median-rtt: 164ms    max-rtt: 620ms
clients: 18000    95per-rtt: 400ms    min-rtt:  62ms    median-rtt: 168ms    max-rtt: 758ms
clients: 19000    95per-rtt: 499ms    min-rtt:  69ms    median-rtt: 168ms    max-rtt: 617ms
clients: 20000    95per-rtt: 451ms    min-rtt:  69ms    median-rtt: 167ms    max-rtt: 976ms
clients: 21000    95per-rtt: 475ms    min-rtt:  69ms    median-rtt: 177ms    max-rtt: 800ms
```

## Raw Go bench / Binary Results

### C++ / uWebSockets -- 1 thread

Server Command

```
dev@earth:~/hashrocket/websocket-shootout/cpp/uwebsockets(master*)% bin/cpp-uwebsockets-server --port 5000 --data-type binary
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+*)% bin/websocket-bench broadcast ws://192.168.50.12:5000/ \
  -l 192.168.50.5 \
  -l 192.168.50.246 \
  -l 192.168.50.247 \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --server-type binary \
  -w neptune:4001 \
  -w uranus:4000 \
  -w saturn:4002 \
  -w venus:4000
clients: 10000    95per-rtt: 201ms    min-rtt:  33ms    median-rtt: 113ms    max-rtt: 242ms
clients: 11000    95per-rtt: 155ms    min-rtt:  40ms    median-rtt: 154ms    max-rtt: 161ms
clients: 12000    95per-rtt: 170ms    min-rtt:  44ms    median-rtt: 168ms    max-rtt: 178ms
clients: 13000    95per-rtt: 190ms    min-rtt:  47ms    median-rtt: 182ms    max-rtt: 354ms
clients: 14000    95per-rtt: 198ms    min-rtt:  51ms    median-rtt: 196ms    max-rtt: 205ms
clients: 15000    95per-rtt: 213ms    min-rtt:  54ms    median-rtt: 211ms    max-rtt: 219ms
clients: 16000    95per-rtt: 229ms    min-rtt:  58ms    median-rtt: 225ms    max-rtt: 235ms
clients: 17000    95per-rtt: 251ms    min-rtt:  58ms    median-rtt: 207ms    max-rtt: 456ms
clients: 18000    95per-rtt: 262ms    min-rtt:  66ms    median-rtt: 253ms    max-rtt: 537ms
clients: 19000    95per-rtt: 280ms    min-rtt:  61ms    median-rtt: 267ms    max-rtt: 374ms
clients: 20000    95per-rtt: 285ms    min-rtt:  75ms    median-rtt: 281ms    max-rtt: 325ms
clients: 21000    95per-rtt: 355ms    min-rtt:  68ms    median-rtt: 295ms    max-rtt: 546ms
clients: 22000    95per-rtt: 321ms    min-rtt:  82ms    median-rtt: 309ms    max-rtt: 357ms
clients: 23000    95per-rtt: 419ms    min-rtt:  80ms    median-rtt: 296ms    max-rtt: 590ms
clients: 24000    95per-rtt: 407ms    min-rtt:  82ms    median-rtt: 337ms    max-rtt: 409ms
clients: 25000    95per-rtt: 357ms    min-rtt: 110ms    median-rtt: 351ms    max-rtt: 380ms
clients: 26000    95per-rtt: 392ms    min-rtt:  94ms    median-rtt: 366ms    max-rtt: 426ms
clients: 27000    95per-rtt: 412ms    min-rtt:  68ms    median-rtt: 379ms    max-rtt: 587ms
clients: 28000    95per-rtt: 397ms    min-rtt: 102ms    median-rtt: 394ms    max-rtt: 416ms
clients: 29000    95per-rtt: 418ms    min-rtt:  91ms    median-rtt: 320ms    max-rtt: 531ms
clients: 30000    95per-rtt: 455ms    min-rtt:  48ms    median-rtt: 421ms    max-rtt: 576ms
```


### C++ / WebsocketPP -- 8 thread

Server Command

```
dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% bin/cpp-websocketpp-server --thread 8 --address 0.0.0.0 --port 5000 --data-type binary
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+*)% bin/websocket-bench broadcast ws://192.168.50.12:5000/ \
  -l 192.168.50.5 \
  -l 192.168.50.246 \
  -l 192.168.50.247 \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --server-type binary \
  -w neptune:4001 \
  -w uranus:4000 \
  -w saturn:4002 \
  -w venus:4000
clients: 10000    95per-rtt: 241ms    min-rtt:  31ms    median-rtt:  72ms    max-rtt: 373ms
clients: 11000    95per-rtt: 248ms    min-rtt:  37ms    median-rtt:  81ms    max-rtt: 344ms
clients: 12000    95per-rtt: 251ms    min-rtt:  39ms    median-rtt:  96ms    max-rtt: 411ms
clients: 13000    95per-rtt: 253ms    min-rtt:  42ms    median-rtt:  89ms    max-rtt: 324ms
clients: 14000    95per-rtt: 254ms    min-rtt:  47ms    median-rtt: 101ms    max-rtt: 360ms
clients: 15000    95per-rtt: 385ms    min-rtt:  49ms    median-rtt:  91ms    max-rtt: 464ms
clients: 16000    95per-rtt: 302ms    min-rtt:  51ms    median-rtt: 126ms    max-rtt: 620ms
clients: 17000    95per-rtt: 322ms    min-rtt:  51ms    median-rtt: 109ms    max-rtt: 783ms
clients: 18000    95per-rtt: 397ms    min-rtt:  58ms    median-rtt: 128ms    max-rtt: 463ms
clients: 19000    95per-rtt: 319ms    min-rtt:  59ms    median-rtt: 120ms    max-rtt: 643ms
clients: 20000    95per-rtt: 361ms    min-rtt:  67ms    median-rtt: 133ms    max-rtt: 558ms
clients: 21000    95per-rtt: 426ms    min-rtt:  69ms    median-rtt: 162ms    max-rtt: 485ms
clients: 22000    95per-rtt: 447ms    min-rtt:  71ms    median-rtt: 149ms    max-rtt: 495ms
clients: 23000    95per-rtt: 405ms    min-rtt:  75ms    median-rtt: 152ms    max-rtt: 510ms
clients: 24000    95per-rtt: 417ms    min-rtt:  78ms    median-rtt: 159ms    max-rtt: 739ms
```

### Go / Websocket

Server Command

```
dev@earth:~/hashrocket/websocket-shootout(master*)% bin/go-websocket-server -address 0.0.0.0 -port 5000
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+*)% bin/websocket-bench broadcast ws://192.168.50.12:5000/binary \
  -l 192.168.50.5 \
  -l 192.168.50.246 \
  -l 192.168.50.247 \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --server-type binary \
  -w neptune:4001 \
  -w uranus:4000 \
  -w saturn:4002 \
  -w venus:4000
clients: 10000    95per-rtt: 245ms    min-rtt:  33ms    median-rtt:  67ms    max-rtt: 286ms
clients: 11000    95per-rtt: 237ms    min-rtt:  35ms    median-rtt:  81ms    max-rtt: 292ms
clients: 12000    95per-rtt: 272ms    min-rtt:  38ms    median-rtt:  76ms    max-rtt: 572ms
clients: 13000    95per-rtt: 290ms    min-rtt:  42ms    median-rtt:  91ms    max-rtt: 442ms
clients: 14000    95per-rtt: 274ms    min-rtt:  44ms    median-rtt: 107ms    max-rtt: 302ms
clients: 15000    95per-rtt: 348ms    min-rtt:  47ms    median-rtt: 107ms    max-rtt: 433ms
clients: 16000    95per-rtt: 289ms    min-rtt:  50ms    median-rtt: 116ms    max-rtt: 468ms
clients: 17000    95per-rtt: 286ms    min-rtt:  52ms    median-rtt: 146ms    max-rtt: 412ms
clients: 18000    95per-rtt: 362ms    min-rtt:  58ms    median-rtt: 132ms    max-rtt: 501ms
clients: 19000    95per-rtt: 345ms    min-rtt:  60ms    median-rtt: 141ms    max-rtt: 611ms
clients: 20000    95per-rtt: 343ms    min-rtt:  63ms    median-rtt: 148ms    max-rtt: 451ms
clients: 21000    95per-rtt: 416ms    min-rtt:  67ms    median-rtt: 163ms    max-rtt: 514ms
clients: 22000    95per-rtt: 434ms    min-rtt:  68ms    median-rtt: 177ms    max-rtt: 759ms
clients: 23000    95per-rtt: 403ms    min-rtt:  72ms    median-rtt: 170ms    max-rtt: 744ms
clients: 24000    95per-rtt: 459ms    min-rtt:  75ms    median-rtt: 180ms    max-rtt: 788ms
clients: 25000    95per-rtt: 404ms    min-rtt:  79ms    median-rtt: 166ms    max-rtt: 760ms
clients: 26000    95per-rtt: 479ms    min-rtt:  83ms    median-rtt: 172ms    max-rtt: 686ms
```

## Raw C++ bench / Binary Results

### C++ / uWebSockets -- 1 thread

Server Command

```
dev@earth:~/hashrocket/websocket-shootout/cpp/uwebsockets(master*)% bin/cpp-uwebsockets-server --port 5000 --data-type binary
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout/cpp/bench(master+*)% bin/cpp-bench \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --address 192.168.50.12 \
  --port 5000 \
  --path /
clients: 10000    95per-rtt: 258ms    min-rtt: 35ms    median-rtt: 144ms    max-rtt: 488ms
clients: 11000    95per-rtt: 251ms    min-rtt: 41ms    median-rtt: 156ms    max-rtt: 527ms
clients: 12000    95per-rtt: 208ms    min-rtt: 45ms    median-rtt: 170ms    max-rtt: 233ms
clients: 13000    95per-rtt: 478ms    min-rtt: 48ms    median-rtt: 185ms    max-rtt: 563ms
clients: 14000    95per-rtt: 224ms    min-rtt: 61ms    median-rtt: 199ms    max-rtt: 240ms
clients: 15000    95per-rtt: 239ms    min-rtt: 61ms    median-rtt: 211ms    max-rtt: 259ms
clients: 16000    95per-rtt: 345ms    min-rtt: 57ms    median-rtt: 227ms    max-rtt: 505ms
clients: 17000    95per-rtt: 445ms    min-rtt: 59ms    median-rtt: 238ms    max-rtt: 629ms
clients: 18000    95per-rtt: 299ms    min-rtt: 67ms    median-rtt: 254ms    max-rtt: 312ms
clients: 19000    95per-rtt: 499ms    min-rtt: 71ms    median-rtt: 269ms    max-rtt: 575ms
clients: 20000    95per-rtt: 484ms    min-rtt: 77ms    median-rtt: 283ms    max-rtt: 724ms
clients: 21000    95per-rtt: 400ms    min-rtt: 79ms    median-rtt: 296ms    max-rtt: 621ms
clients: 22000    95per-rtt: 351ms    min-rtt: 82ms    median-rtt: 311ms    max-rtt: 375ms
clients: 23000    95per-rtt: 374ms    min-rtt: 87ms    median-rtt: 324ms    max-rtt: 452ms
clients: 24000    95per-rtt: 463ms    min-rtt: 91ms    median-rtt: 348ms    max-rtt: 711ms
clients: 25000    95per-rtt: 450ms    min-rtt: 95ms    median-rtt: 354ms    max-rtt: 513ms
```


### C++ / WebsocketPP -- 8 thread

Note: Performance of this server was highly variable in this particular benchmark. Other runs had substantially different results.

Server Command

```
dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% bin/cpp-websocketpp-server --thread 8 --address 0.0.0.0 --port 5000 --data-type binary
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout/cpp/bench(master+*)% bin/cpp-bench \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --address 192.168.50.12 \
  --port 5000 \
  --path /
clients: 10000    95per-rtt: 172ms    min-rtt: 132ms    median-rtt: 168ms    max-rtt: 173ms
clients: 11000    95per-rtt: 191ms    min-rtt: 146ms    median-rtt: 188ms    max-rtt: 192ms
clients: 12000    95per-rtt: 390ms    min-rtt: 159ms    median-rtt: 206ms    max-rtt: 394ms
clients: 13000    95per-rtt: 456ms    min-rtt: 169ms    median-rtt: 226ms    max-rtt: 763ms
clients: 14000    95per-rtt: 253ms    min-rtt: 183ms    median-rtt: 246ms    max-rtt: 423ms
clients: 15000    95per-rtt: 464ms    min-rtt: 199ms    median-rtt: 264ms    max-rtt: 765ms
clients: 16000    95per-rtt: 482ms    min-rtt: 210ms    median-rtt: 285ms    max-rtt: 484ms
```

### Go / Websocket

Server Command

```
dev@earth:~/hashrocket/websocket-shootout(master*)% bin/go-websocket-server -address 0.0.0.0 -port 5000
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout/cpp/bench(master+*)% bin/cpp-bench \
  --concurrent 4 \
  --sample-size 100 \
  --initial-clients 10000 \
  --step-size 1000 \
  --payload-padding 200 \
  --address 192.168.50.12 \
  --port 5000 \
  --path /binary
clients: 10000    95per-rtt: 440ms    min-rtt: 48ms    median-rtt: 244ms    max-rtt: 486ms
clients: 11000    95per-rtt: 457ms    min-rtt: 75ms    median-rtt: 287ms    max-rtt: 509ms
clients: 12000    95per-rtt: 452ms    min-rtt: 117ms    median-rtt: 258ms    max-rtt: 676ms
clients: 13000    95per-rtt: 500ms    min-rtt: 62ms    median-rtt: 418ms    max-rtt: 649ms
clients: 14000    95per-rtt: 449ms    min-rtt: 70ms    median-rtt: 263ms    max-rtt: 472ms
clients: 15000    95per-rtt: 474ms    min-rtt: 80ms    median-rtt: 262ms    max-rtt: 512ms
clients: 16000    95per-rtt: 461ms    min-rtt: 79ms    median-rtt: 314ms    max-rtt: 516ms
```
