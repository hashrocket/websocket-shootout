## Round 2

Round 2 results are still in-progress.

These results are from running the server on one machine and the benchmark tool as another. Both machines are bare metal 4ghz i7 4790Ks with 16GB of RAM running Ubuntu 16.04 connected via GB ethernet. Tests were run multiple times and the best results were recorded.

## Clojure

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/clojure/httpkit(master*)% java -version
java version "1.8.0_101"
Java(TM) SE Runtime Environment (build 1.8.0_101-b13)
Java HotSpot(TM) 64-Bit Server VM (build 25.101-b13, mixed mode)
dev@earth:~/hashrocket/websocket-shootout/clojure/httpkit(master*)% java -server -XX:+AggressiveOpts -XX:+UseG1GC -XX:MaxGCPauseMillis=50 -Xms8g -Xmx8g -jar build/app.jar
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master)% bin/websocket-bench broadcast ws://192.168.50.12:3000/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w ne
ptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  42ms    min-rtt:   5ms    median-rtt:  14ms    max-rtt:  44ms
clients:  2000    95per-rtt:  48ms    min-rtt:   9ms    median-rtt:  19ms    max-rtt:  76ms
clients:  3000    95per-rtt: 166ms    min-rtt:  14ms    median-rtt:  26ms    max-rtt: 263ms
clients:  4000    95per-rtt: 189ms    min-rtt:  15ms    median-rtt:  36ms    max-rtt: 299ms
clients:  5000    95per-rtt: 216ms    min-rtt:  21ms    median-rtt:  36ms    max-rtt: 241ms
clients:  6000    95per-rtt: 245ms    min-rtt:  22ms    median-rtt:  51ms    max-rtt: 514ms
clients:  7000    95per-rtt: 225ms    min-rtt:  24ms    median-rtt:  60ms    max-rtt: 295ms
clients:  8000    95per-rtt: 231ms    min-rtt:  26ms    median-rtt:  81ms    max-rtt: 276ms
clients:  9000    95per-rtt: 254ms    min-rtt:  37ms    median-rtt:  83ms    max-rtt: 451ms
clients: 10000    95per-rtt: 233ms    min-rtt:  40ms    median-rtt: 101ms    max-rtt: 394ms
clients: 11000    95per-rtt: 311ms    min-rtt:  36ms    median-rtt:  79ms    max-rtt: 469ms
clients: 12000    95per-rtt: 359ms    min-rtt:  39ms    median-rtt:  78ms    max-rtt: 428ms
clients: 13000    95per-rtt: 451ms    min-rtt:  39ms    median-rtt: 113ms    max-rtt: 709ms
clients: 14000    95per-rtt: 429ms    min-rtt:  44ms    median-rtt: 147ms    max-rtt: 658ms
clients: 15000    95per-rtt: 499ms    min-rtt:  45ms    median-rtt: 157ms    max-rtt: 676ms
clients: 16000    95per-rtt: 440ms    min-rtt:  52ms    median-rtt: 168ms    max-rtt: 671ms
clients: 17000    95per-rtt: 500ms    min-rtt:  50ms    median-rtt: 176ms    max-rtt: 620ms
clients: 18000    95per-rtt: 436ms    min-rtt:  54ms    median-rtt: 173ms    max-rtt: 1403ms
```

## C++ / uWebSockets / RapidJSON -- 1 thread

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/cpp/uwebsockets(master*)% gcc -v                                                                                                                                                               {130}
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)

dev@earth:~/hashrocket/websocket-shootout/cpp/uwebsockets(master*)% bin/cpp-uwebsockets-server -p 3334
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -
w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  38ms    min-rtt:   4ms    median-rtt:  10ms    max-rtt:  42ms
clients:  2000    95per-rtt:  50ms    min-rtt:   7ms    median-rtt:  17ms    max-rtt: 223ms
clients:  3000    95per-rtt: 201ms    min-rtt:   9ms    median-rtt:  43ms    max-rtt: 218ms
clients:  4000    95per-rtt:  61ms    min-rtt:  16ms    median-rtt:  55ms    max-rtt:  86ms
clients:  5000    95per-rtt:  84ms    min-rtt:  20ms    median-rtt:  73ms    max-rtt:  93ms
clients:  6000    95per-rtt:  93ms    min-rtt:  29ms    median-rtt:  87ms    max-rtt:  97ms
clients:  7000    95per-rtt: 107ms    min-rtt:  27ms    median-rtt: 102ms    max-rtt: 109ms
clients:  8000    95per-rtt: 126ms    min-rtt:  34ms    median-rtt: 116ms    max-rtt: 137ms
clients:  9000    95per-rtt: 136ms    min-rtt:  69ms    median-rtt: 131ms    max-rtt: 143ms
clients: 10000    95per-rtt: 152ms    min-rtt:  39ms    median-rtt: 145ms    max-rtt: 156ms
clients: 11000    95per-rtt: 170ms    min-rtt:  53ms    median-rtt: 160ms    max-rtt: 175ms
clients: 12000    95per-rtt: 182ms    min-rtt:  50ms    median-rtt: 174ms    max-rtt: 201ms
clients: 13000    95per-rtt: 348ms    min-rtt:  36ms    median-rtt: 125ms    max-rtt: 432ms
clients: 14000    95per-rtt: 300ms    min-rtt:  39ms    median-rtt: 205ms    max-rtt: 419ms
clients: 15000    95per-rtt: 235ms    min-rtt:  60ms    median-rtt: 221ms    max-rtt: 251ms
clients: 16000    95per-rtt: 250ms    min-rtt:  40ms    median-rtt: 225ms    max-rtt: 712ms
clients: 17000    95per-rtt: 264ms    min-rtt:  68ms    median-rtt: 248ms    max-rtt: 278ms
clients: 18000    95per-rtt: 288ms    min-rtt:  81ms    median-rtt: 264ms    max-rtt: 332ms
clients: 19000    95per-rtt: 289ms    min-rtt:  76ms    median-rtt: 276ms    max-rtt: 306ms
clients: 20000    95per-rtt: 477ms    min-rtt:  53ms    median-rtt: 258ms    max-rtt: 829ms
clients: 21000    95per-rtt: 365ms    min-rtt:  59ms    median-rtt: 305ms    max-rtt: 746ms
clients: 22000    95per-rtt: 332ms    min-rtt:  85ms    median-rtt: 321ms    max-rtt: 351ms
```

## C++ / WebsocketPP / jsoncpp -- 1 thread

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% gcc -v
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)

dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% bin/cpp-websocketpp-server --thread 1 --address 0.0.0.0 --port 3334

```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:3334 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  21ms    min-rtt:   5ms    median-rtt:  14ms    max-rtt:  23ms
clients:  2000    95per-rtt:  51ms    min-rtt:   8ms    median-rtt:  30ms    max-rtt:  60ms
clients:  3000    95per-rtt:  82ms    min-rtt:  18ms    median-rtt:  48ms    max-rtt:  93ms
clients:  4000    95per-rtt: 139ms    min-rtt:  27ms    median-rtt:  73ms    max-rtt: 186ms
clients:  5000    95per-rtt: 183ms    min-rtt:  33ms    median-rtt:  92ms    max-rtt: 234ms
clients:  6000    95per-rtt: 262ms    min-rtt:  43ms    median-rtt: 102ms    max-rtt: 389ms
clients:  7000    95per-rtt: 334ms    min-rtt:  52ms    median-rtt: 123ms    max-rtt: 383ms
clients:  8000    95per-rtt: 326ms    min-rtt:  54ms    median-rtt: 149ms    max-rtt: 430ms
clients:  9000    95per-rtt: 358ms    min-rtt:  71ms    median-rtt: 168ms    max-rtt: 437ms
clients: 10000    95per-rtt: 392ms    min-rtt:  83ms    median-rtt: 174ms    max-rtt: 485ms
clients: 11000    95per-rtt: 408ms    min-rtt:  63ms    median-rtt: 213ms    max-rtt: 619ms
clients: 12000    95per-rtt: 489ms    min-rtt:  82ms    median-rtt: 228ms    max-rtt: 681ms
clients: 13000    95per-rtt: 449ms    min-rtt: 102ms    median-rtt: 255ms    max-rtt: 832ms
```

## C++ / WebsocketPP / jsoncpp -- 8 thread

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% gcc -v
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)

dev@earth:~/hashrocket/websocket-shootout/cpp/websocketpp(master*)% bin/cpp-websocketpp-server --thread 8 --address 0.0.0.0 --port 3334

```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:3334 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sampl
e-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  23ms    min-rtt:   6ms    median-rtt:  12ms    max-rtt:  26ms
clients:  2000    95per-rtt:  43ms    min-rtt:  12ms    median-rtt:  22ms    max-rtt: 202ms
clients:  3000    95per-rtt: 203ms    min-rtt:  17ms    median-rtt:  28ms    max-rtt: 249ms
clients:  4000    95per-rtt: 158ms    min-rtt:  19ms    median-rtt:  39ms    max-rtt: 171ms
clients:  5000    95per-rtt: 242ms    min-rtt:  22ms    median-rtt:  46ms    max-rtt: 417ms
clients:  6000    95per-rtt: 214ms    min-rtt:  29ms    median-rtt:  59ms    max-rtt: 403ms
clients:  7000    95per-rtt: 255ms    min-rtt:  30ms    median-rtt:  75ms    max-rtt: 453ms
clients:  8000    95per-rtt: 256ms    min-rtt:  34ms    median-rtt:  83ms    max-rtt: 294ms
clients:  9000    95per-rtt: 288ms    min-rtt:  36ms    median-rtt:  74ms    max-rtt: 426ms
clients: 10000    95per-rtt: 282ms    min-rtt:  42ms    median-rtt: 100ms    max-rtt: 467ms
clients: 11000    95per-rtt: 306ms    min-rtt:  42ms    median-rtt: 107ms    max-rtt: 421ms
clients: 12000    95per-rtt: 328ms    min-rtt:  49ms    median-rtt: 106ms    max-rtt: 622ms
clients: 13000    95per-rtt: 399ms    min-rtt:  51ms    median-rtt: 127ms    max-rtt: 594ms
clients: 14000    95per-rtt: 322ms    min-rtt:  56ms    median-rtt: 130ms    max-rtt: 622ms
clients: 15000    95per-rtt: 482ms    min-rtt:  61ms    median-rtt: 158ms    max-rtt: 737ms
clients: 16000    95per-rtt: 413ms    min-rtt:  62ms    median-rtt: 173ms    max-rtt: 552ms
clients: 17000    95per-rtt: 466ms    min-rtt:  66ms    median-rtt: 166ms    max-rtt: 616ms
```


## Elixir / Phoenix

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/elixir/phoenix_socket(master*)% elixir -v
Erlang/OTP 19 [erts-8.1] [source-77fb4f8] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Elixir 1.3.2
dev@earth:~/hashrocket/websocket-shootout/elixir/phoenix_socket(master*)% mix_ENV=prod mix phoenix.server
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master+)% time bin/websocket-bench broadcast ws://192.168.50.12:4000/socket/websocket -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000 --server-type phoenix
clients:  1000    95per-rtt:  19ms    min-rtt:   6ms    median-rtt:  12ms    max-rtt:  22ms
clients:  2000    95per-rtt:  69ms    min-rtt:  11ms    median-rtt:  19ms    max-rtt: 135ms
clients:  3000    95per-rtt: 115ms    min-rtt:  17ms    median-rtt:  27ms    max-rtt: 228ms
clients:  4000    95per-rtt: 164ms    min-rtt:  20ms    median-rtt:  42ms    max-rtt: 234ms
clients:  5000    95per-rtt: 229ms    min-rtt:  22ms    median-rtt:  50ms    max-rtt: 429ms
clients:  6000    95per-rtt: 220ms    min-rtt:  26ms    median-rtt:  57ms    max-rtt: 279ms
clients:  7000    95per-rtt: 235ms    min-rtt:  29ms    median-rtt:  62ms    max-rtt: 388ms
clients:  8000    95per-rtt: 257ms    min-rtt:  32ms    median-rtt:  76ms    max-rtt: 493ms
clients:  9000    95per-rtt: 270ms    min-rtt:  35ms    median-rtt: 108ms    max-rtt: 457ms
clients: 10000    95per-rtt: 352ms    min-rtt:  38ms    median-rtt: 104ms    max-rtt: 436ms
clients: 11000    95per-rtt: 318ms    min-rtt:  43ms    median-rtt: 125ms    max-rtt: 1037ms
clients: 12000    95per-rtt: 383ms    min-rtt:  44ms    median-rtt: 133ms    max-rtt: 518ms
clients: 13000    95per-rtt: 465ms    min-rtt:  47ms    median-rtt: 134ms    max-rtt: 552ms
clients: 14000    95per-rtt: 458ms    min-rtt:  51ms    median-rtt: 137ms    max-rtt: 585ms
clients: 15000    95per-rtt: 430ms    min-rtt:  53ms    median-rtt: 155ms    max-rtt: 665ms
```
