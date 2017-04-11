## Round 2

This test differs from round 1 in several ways. First, the test is parallized across multiple clients. This should avoid the possibility of a servers performance being limited by the benchmark. Second, the number of broadcasts per sample was increased from 40 to 100. Thirdly, 200 bytes of padding was added to the payload. This should make the message size more realistic.

These results are from running the server on one machine and the benchmark tool parallelized across multiple other machines. The server is a bare metal 4ghz i7 4790K with 16GB of RAM running Ubuntu 16.04 connected via GB ethernet to the benchmark machines. Tests were run multiple times and the best results were recorded.

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
dev@mercury:~/hashrocket/websocket-shootout(master)% bin/websocket-bench broadcast ws://192.168.50.12:3000/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
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

## C++ / Crow / RapidJSON -- 1 thread

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/cpp/crow(master*)% gcc -v
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)

dev@earth:~/hashrocket/websocket-shootout/cpp/crow(master*)% bin/cpp-crow-server --port 3334 --thread 1
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  40ms    min-rtt:   4ms    median-rtt:  11ms    max-rtt:  44ms
clients:  2000    95per-rtt:  47ms    min-rtt:   8ms    median-rtt:  17ms    max-rtt:  59ms
clients:  3000    95per-rtt:  66ms    min-rtt:  12ms    median-rtt:  29ms    max-rtt: 144ms
clients:  4000    95per-rtt:  93ms    min-rtt:  14ms    median-rtt:  38ms    max-rtt: 182ms
clients:  5000    95per-rtt: 127ms    min-rtt:  17ms    median-rtt:  44ms    max-rtt: 210ms
clients:  6000    95per-rtt: 122ms    min-rtt:  18ms    median-rtt:  61ms    max-rtt: 204ms
clients:  7000    95per-rtt: 133ms    min-rtt:  31ms    median-rtt: 117ms    max-rtt: 140ms
clients:  8000    95per-rtt: 194ms    min-rtt:  46ms    median-rtt: 167ms    max-rtt: 197ms
clients:  9000    95per-rtt: 184ms    min-rtt:  37ms    median-rtt:  86ms    max-rtt: 361ms
clients: 10000    95per-rtt: 152ms    min-rtt:  33ms    median-rtt: 122ms    max-rtt: 299ms
clients: 11000    95per-rtt: 158ms    min-rtt:  52ms    median-rtt: 135ms    max-rtt: 338ms
clients: 12000    95per-rtt: 252ms    min-rtt: 108ms    median-rtt: 204ms    max-rtt: 267ms
clients: 13000    95per-rtt: 266ms    min-rtt:  59ms    median-rtt: 157ms    max-rtt: 351ms
clients: 14000    95per-rtt: 184ms    min-rtt:  42ms    median-rtt: 173ms    max-rtt: 226ms
clients: 15000    95per-rtt: 216ms    min-rtt:  46ms    median-rtt: 184ms    max-rtt: 301ms
clients: 16000    95per-rtt: 212ms    min-rtt:  85ms    median-rtt: 194ms    max-rtt: 249ms
clients: 17000    95per-rtt: 276ms    min-rtt:  86ms    median-rtt: 230ms    max-rtt: 330ms
clients: 18000    95per-rtt: 451ms    min-rtt: 158ms    median-rtt: 342ms    max-rtt: 471ms
clients: 19000    95per-rtt: 424ms    min-rtt: 186ms    median-rtt: 344ms    max-rtt: 460ms
clients: 20000    95per-rtt: 293ms    min-rtt: 114ms    median-rtt: 279ms    max-rtt: 362ms
```

## C++ / Crow / RapidJSON -- 8 threads

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/cpp/crow(master*)% gcc -v
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)

dev@earth:~/hashrocket/websocket-shootout/cpp/crow(master*)% bin/cpp-crow-server --port 3334 --thread 8
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  41ms    min-rtt:   4ms    median-rtt:  12ms    max-rtt:  45ms
clients:  2000    95per-rtt:  51ms    min-rtt:   7ms    median-rtt:  20ms    max-rtt: 212ms
clients:  3000    95per-rtt: 127ms    min-rtt:   9ms    median-rtt:  28ms    max-rtt: 212ms
clients:  4000    95per-rtt: 209ms    min-rtt:  10ms    median-rtt:  33ms    max-rtt: 234ms
clients:  5000    95per-rtt: 195ms    min-rtt:  15ms    median-rtt:  44ms    max-rtt: 326ms
clients:  6000    95per-rtt: 226ms    min-rtt:  16ms    median-rtt:  33ms    max-rtt: 502ms
clients:  7000    95per-rtt: 236ms    min-rtt:  17ms    median-rtt:  42ms    max-rtt: 470ms
clients:  8000    95per-rtt: 309ms    min-rtt:  19ms    median-rtt:  42ms    max-rtt: 638ms
clients:  9000    95per-rtt: 361ms    min-rtt:  21ms    median-rtt:  40ms    max-rtt: 850ms
clients: 10000    95per-rtt: 421ms    min-rtt:  22ms    median-rtt:  50ms    max-rtt: 657ms
clients: 11000    95per-rtt: 381ms    min-rtt:  23ms    median-rtt:  57ms    max-rtt: 516ms
clients: 12000    95per-rtt: 326ms    min-rtt:  27ms    median-rtt:  73ms    max-rtt: 752ms
clients: 13000    95per-rtt: 420ms    min-rtt:  30ms    median-rtt:  46ms    max-rtt: 857ms
clients: 14000    95per-rtt: 498ms    min-rtt:  29ms    median-rtt:  59ms    max-rtt: 667ms
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
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 -[72/2175]
e 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  35ms    min-rtt:   4ms    median-rtt:  10ms    max-rtt:  42ms
clients:  2000    95per-rtt:  47ms    min-rtt:   8ms    median-rtt:  17ms    max-rtt: 209ms
clients:  3000    95per-rtt: 103ms    min-rtt:  12ms    median-rtt:  36ms    max-rtt: 175ms
clients:  4000    95per-rtt:  62ms    min-rtt:  16ms    median-rtt:  58ms    max-rtt:  82ms
clients:  5000    95per-rtt:  92ms    min-rtt:  19ms    median-rtt:  73ms    max-rtt: 256ms
clients:  6000    95per-rtt:  91ms    min-rtt:  23ms    median-rtt:  87ms    max-rtt: 101ms
clients:  7000    95per-rtt: 127ms    min-rtt:  27ms    median-rtt:  96ms    max-rtt: 247ms
clients:  8000    95per-rtt: 124ms    min-rtt:  31ms    median-rtt: 116ms    max-rtt: 127ms
clients:  9000    95per-rtt: 204ms    min-rtt:  30ms    median-rtt: 130ms    max-rtt: 458ms
clients: 10000    95per-rtt: 151ms    min-rtt:  49ms    median-rtt: 145ms    max-rtt: 154ms
clients: 11000    95per-rtt: 397ms    min-rtt:  28ms    median-rtt: 121ms    max-rtt: 747ms
clients: 12000    95per-rtt: 186ms    min-rtt:  47ms    median-rtt: 175ms    max-rtt: 188ms
clients: 13000    95per-rtt: 200ms    min-rtt:  52ms    median-rtt: 189ms    max-rtt: 213ms
clients: 14000    95per-rtt: 405ms    min-rtt:  38ms    median-rtt: 105ms    max-rtt: 748ms
clients: 15000    95per-rtt: 236ms    min-rtt:  50ms    median-rtt: 218ms    max-rtt: 332ms
clients: 16000    95per-rtt: 284ms    min-rtt:  47ms    median-rtt: 233ms    max-rtt: 425ms
clients: 17000    95per-rtt: 376ms    min-rtt:  43ms    median-rtt: 235ms    max-rtt: 513ms
clients: 18000    95per-rtt: 411ms    min-rtt:  45ms    median-rtt: 187ms    max-rtt: 880ms
clients: 19000    95per-rtt: 338ms    min-rtt:  59ms    median-rtt: 271ms    max-rtt: 690ms
clients: 20000    95per-rtt: 454ms    min-rtt:  55ms    median-rtt: 221ms    max-rtt: 735ms
clients: 21000    95per-rtt: 326ms    min-rtt:  92ms    median-rtt: 306ms    max-rtt: 393ms
clients: 22000    95per-rtt: 336ms    min-rtt:  85ms    median-rtt: 320ms    max-rtt: 446ms
clients: 23000    95per-rtt: 351ms    min-rtt: 101ms    median-rtt: 335ms    max-rtt: 413ms
clients: 24000    95per-rtt: 364ms    min-rtt:  93ms    median-rtt: 350ms    max-rtt: 376ms
```

## C++ / WebsocketPP / jsoncpp -- 1 thread

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

## Elixir / Plug

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/elixir/plug_socket(master*)% elixir -v
Erlang/OTP 19 [erts-8.1] [source-77fb4f8] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Elixir 1.3.2

dev@earth:~/hashrocket/websocket-shootout/elixir/plug_socket(master*)% MIX_ENV=prod mix do compile, run --no-halt
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:4000/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-s
ize 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  19ms    min-rtt:   2ms    median-rtt:  11ms    max-rtt:  27ms
clients:  2000    95per-rtt: 197ms    min-rtt:   5ms    median-rtt:  16ms    max-rtt: 221ms
clients:  3000    95per-rtt: 203ms    min-rtt:  11ms    median-rtt:  26ms    max-rtt: 294ms
clients:  4000    95per-rtt: 214ms    min-rtt:   9ms    median-rtt:  37ms    max-rtt: 226ms
clients:  5000    95per-rtt: 227ms    min-rtt:  14ms    median-rtt:  52ms    max-rtt: 415ms
clients:  6000    95per-rtt: 258ms    min-rtt:  14ms    median-rtt:  47ms    max-rtt: 543ms
clients:  7000    95per-rtt: 232ms    min-rtt:  20ms    median-rtt:  65ms    max-rtt: 529ms
clients:  8000    95per-rtt: 246ms    min-rtt:  20ms    median-rtt: 116ms    max-rtt: 445ms
clients:  9000    95per-rtt: 243ms    min-rtt:  26ms    median-rtt:  96ms    max-rtt: 280ms
clients: 10000    95per-rtt: 326ms    min-rtt:  27ms    median-rtt: 141ms    max-rtt: 622ms
clients: 11000    95per-rtt: 362ms    min-rtt:  26ms    median-rtt: 106ms    max-rtt: 475ms
clients: 12000    95per-rtt: 406ms    min-rtt:  28ms    median-rtt: 154ms    max-rtt: 644ms
clients: 13000    95per-rtt: 462ms    min-rtt:  33ms    median-rtt: 140ms    max-rtt: 1022ms
clients: 14000    95per-rtt: 445ms    min-rtt:  36ms    median-rtt: 147ms    max-rtt: 869ms
clients: 15000    95per-rtt: 449ms    min-rtt:  37ms    median-rtt: 152ms    max-rtt: 805ms
clients: 16000    95per-rtt: 497ms    min-rtt:  37ms    median-rtt: 146ms    max-rtt: 1454ms
clients: 17000    95per-rtt: 476ms    min-rtt:  40ms    median-rtt: 158ms    max-rtt: 1386ms
clients: 18000    95per-rtt: 500ms    min-rtt:  42ms    median-rtt: 177ms    max-rtt: 976ms
clients: 19000    95per-rtt: 499ms    min-rtt:  43ms    median-rtt: 171ms    max-rtt: 678ms
```

## Go / Gorilla

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% go version
go version go1.7 linux/amd64

dev@earth:~/hashrocket/websocket-shootout(master*)% go/bin/go-gorilla-server -address 0.0.0.0 -port 400
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:4000/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-s
ize 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  17ms    min-rtt:   7ms    median-rtt:  11ms    max-rtt:  19ms
clients:  2000    95per-rtt:  51ms    min-rtt:  10ms    median-rtt:  18ms    max-rtt: 216ms
clients:  3000    95per-rtt: 155ms    min-rtt:  14ms    median-rtt:  26ms    max-rtt: 194ms
clients:  4000    95per-rtt:  96ms    min-rtt:  16ms    median-rtt:  35ms    max-rtt: 222ms
clients:  5000    95per-rtt: 146ms    min-rtt:  19ms    median-rtt:  42ms    max-rtt: 228ms
clients:  6000    95per-rtt: 209ms    min-rtt:  22ms    median-rtt:  49ms    max-rtt: 269ms
clients:  7000    95per-rtt: 201ms    min-rtt:  25ms    median-rtt:  59ms    max-rtt: 381ms
clients:  8000    95per-rtt: 244ms    min-rtt:  27ms    median-rtt:  63ms    max-rtt: 352ms
clients:  9000    95per-rtt: 336ms    min-rtt:  30ms    median-rtt:  69ms    max-rtt: 438ms
clients: 10000    95per-rtt: 261ms    min-rtt:  35ms    median-rtt:  82ms    max-rtt: 325ms
clients: 11000    95per-rtt: 251ms    min-rtt:  34ms    median-rtt: 105ms    max-rtt: 445ms
clients: 12000    95per-rtt: 327ms    min-rtt:  39ms    median-rtt: 100ms    max-rtt: 386ms
clients: 13000    95per-rtt: 351ms    min-rtt:  42ms    median-rtt:  98ms    max-rtt: 545ms
clients: 14000    95per-rtt: 378ms    min-rtt:  49ms    median-rtt: 115ms    max-rtt: 439ms
clients: 15000    95per-rtt: 439ms    min-rtt:  47ms    median-rtt: 129ms    max-rtt: 663ms
clients: 16000    95per-rtt: 350ms    min-rtt:  49ms    median-rtt: 146ms    max-rtt: 544ms
clients: 17000    95per-rtt: 442ms    min-rtt:  55ms    median-rtt: 139ms    max-rtt: 846ms
clients: 18000    95per-rtt: 479ms    min-rtt:  56ms    median-rtt: 161ms    max-rtt: 882ms
clients: 19000    95per-rtt: 438ms    min-rtt:  59ms    median-rtt: 169ms    max-rtt: 628ms
clients: 20000    95per-rtt: 489ms    min-rtt:  63ms    median-rtt: 190ms    max-rtt: 898ms
```

## Go / Websocket

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% go version
go version go1.7 linux/amd64

dev@earth:~/hashrocket/websocket-shootout(master*)% bin/go-websocket-server -address 0.0.0.0 -port 4000
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:4000/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  18ms    min-rtt:   6ms    median-rtt:  11ms    max-rtt:  23ms
clients:  2000    95per-rtt:  48ms    min-rtt:  10ms    median-rtt:  20ms    max-rtt:  57ms
clients:  3000    95per-rtt: 177ms    min-rtt:  13ms    median-rtt:  26ms    max-rtt: 235ms
clients:  4000    95per-rtt: 199ms    min-rtt:  17ms    median-rtt:  38ms    max-rtt: 226ms
clients:  5000    95per-rtt: 161ms    min-rtt:  21ms    median-rtt:  45ms    max-rtt: 234ms
clients:  6000    95per-rtt: 205ms    min-rtt:  24ms    median-rtt:  56ms    max-rtt: 236ms
clients:  7000    95per-rtt: 341ms    min-rtt:  24ms    median-rtt:  47ms    max-rtt: 1487ms
clients:  8000    95per-rtt: 244ms    min-rtt:  29ms    median-rtt:  70ms    max-rtt: 295ms
clients:  9000    95per-rtt: 194ms    min-rtt:  33ms    median-rtt:  74ms    max-rtt: 531ms
clients: 10000    95per-rtt: 253ms    min-rtt:  37ms    median-rtt:  93ms    max-rtt: 306ms
clients: 11000    95per-rtt: 250ms    min-rtt:  41ms    median-rtt: 106ms    max-rtt: 325ms
clients: 12000    95per-rtt: 280ms    min-rtt:  45ms    median-rtt: 105ms    max-rtt: 405ms
clients: 13000    95per-rtt: 265ms    min-rtt:  46ms    median-rtt: 108ms    max-rtt: 669ms
clients: 14000    95per-rtt: 332ms    min-rtt:  48ms    median-rtt: 115ms    max-rtt: 822ms
clients: 15000    95per-rtt: 300ms    min-rtt:  53ms    median-rtt: 138ms    max-rtt: 475ms
clients: 16000    95per-rtt: 394ms    min-rtt:  55ms    median-rtt: 143ms    max-rtt: 501ms
clients: 17000    95per-rtt: 379ms    min-rtt:  56ms    median-rtt: 142ms    max-rtt: 598ms
clients: 18000    95per-rtt: 389ms    min-rtt:  62ms    median-rtt: 149ms    max-rtt: 749ms
clients: 19000    95per-rtt: 423ms    min-rtt:  66ms    median-rtt: 170ms    max-rtt: 464ms
clients: 20000    95per-rtt: 487ms    min-rtt:  71ms    median-rtt: 186ms    max-rtt: 540ms
clients: 21000    95per-rtt: 466ms    min-rtt:  73ms    median-rtt: 162ms    max-rtt: 703ms
clients: 22000    95per-rtt: 463ms    min-rtt:  79ms    median-rtt: 193ms    max-rtt: 1047ms
```

## Haskell

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% stack --version
Version 1.2.0, Git revision 241cd07d576d9c0c0e712e83d947e3dd64541c42 (4054 commits) x86_64 hpack-0.14.0
dev@earth:~/hashrocket/websocket-shootout(master*)% cat haskell/warp/stack.yaml
resolver: lts-6.14

packages:
- '.'

extra-deps:
- unagi-chan-0.4.0.0

flags: {}

extra-package-dbs: []

dev@earth:~/hashrocket/websocket-shootout(master*)% bin/haskell-warp-ws-server
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3000/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  27ms    min-rtt:   3ms    median-rtt:  10ms    max-rtt:  34ms
clients:  2000    95per-rtt: 111ms    min-rtt:   0ms    median-rtt:  16ms    max-rtt: 127ms
clients:  3000    95per-rtt: 168ms    min-rtt:   0ms    median-rtt:  19ms    max-rtt: 216ms
clients:  4000    95per-rtt: 168ms    min-rtt:   7ms    median-rtt:  22ms    max-rtt: 472ms
clients:  5000    95per-rtt: 228ms    min-rtt:   8ms    median-rtt:  25ms    max-rtt: 482ms
clients:  6000    95per-rtt: 233ms    min-rtt:   7ms    median-rtt:  28ms    max-rtt: 428ms
clients:  7000    95per-rtt: 291ms    min-rtt:   0ms    median-rtt:  29ms    max-rtt: 451ms
clients:  8000    95per-rtt: 261ms    min-rtt:   0ms    median-rtt:  30ms    max-rtt: 423ms
clients:  9000    95per-rtt: 316ms    min-rtt:   7ms    median-rtt:  43ms    max-rtt: 481ms
clients: 10000    95per-rtt: 307ms    min-rtt:   0ms    median-rtt:  60ms    max-rtt: 478ms
clients: 11000    95per-rtt: 469ms    min-rtt:  13ms    median-rtt:  56ms    max-rtt: 720ms
clients: 12000    95per-rtt: 469ms    min-rtt:   1ms    median-rtt:  51ms    max-rtt: 947ms
clients: 13000    95per-rtt: 462ms    min-rtt:   0ms    median-rtt:  65ms    max-rtt: 766ms
```

## Haskell - no chan

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% stack --version
Version 1.2.0, Git revision 241cd07d576d9c0c0e712e83d947e3dd64541c42 (4054 commits) x86_64 hpack-0.14.0
dev@earth:~/hashrocket/websocket-shootout(master*)% cat haskell/warp/stack.yaml
resolver: lts-6.14

packages:
- '.'

extra-deps:
- unagi-chan-0.4.0.0

flags: {}

extra-package-dbs: []

dev@earth:~/hashrocket/websocket-shootout(master*)% bin/haskell-warp-ws-server
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3000/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  33ms    min-rtt:   6ms    median-rtt:  14ms    max-rtt:  52ms
clients:  2000    95per-rtt:  38ms    min-rtt:  12ms    median-rtt:  25ms    max-rtt:  44ms
clients:  3000    95per-rtt:  72ms    min-rtt:  16ms    median-rtt:  35ms    max-rtt: 110ms
clients:  4000    95per-rtt:  80ms    min-rtt:  25ms    median-rtt:  47ms    max-rtt: 129ms
clients:  5000    95per-rtt: 103ms    min-rtt:  30ms    median-rtt:  56ms    max-rtt: 151ms
clients:  6000    95per-rtt: 158ms    min-rtt:  36ms    median-rtt:  61ms    max-rtt: 267ms
clients:  7000    95per-rtt: 162ms    min-rtt:  38ms    median-rtt:  70ms    max-rtt: 247ms
clients:  8000    95per-rtt: 184ms    min-rtt:  43ms    median-rtt:  78ms    max-rtt: 274ms
clients:  9000    95per-rtt: 201ms    min-rtt:  48ms    median-rtt:  90ms    max-rtt: 235ms
clients: 10000    95per-rtt: 169ms    min-rtt:  57ms    median-rtt:  94ms    max-rtt: 327ms
clients: 11000    95per-rtt: 259ms    min-rtt:  73ms    median-rtt: 108ms    max-rtt: 354ms
clients: 12000    95per-rtt: 273ms    min-rtt:  62ms    median-rtt: 121ms    max-rtt: 319ms
clients: 13000    95per-rtt: 280ms    min-rtt:  71ms    median-rtt: 124ms    max-rtt: 311ms
clients: 14000    95per-rtt: 294ms    min-rtt:  76ms    median-rtt: 141ms    max-rtt: 403ms
clients: 15000    95per-rtt: 279ms    min-rtt:  74ms    median-rtt: 137ms    max-rtt: 418ms
clients: 16000    95per-rtt: 333ms    min-rtt:  81ms    median-rtt: 154ms    max-rtt: 591ms
clients: 17000    95per-rtt: 374ms    min-rtt:  85ms    median-rtt: 164ms    max-rtt: 548ms
clients: 18000    95per-rtt: 332ms    min-rtt: 111ms    median-rtt: 172ms    max-rtt: 476ms
clients: 19000    95per-rtt: 381ms    min-rtt: 114ms    median-rtt: 191ms    max-rtt: 479ms
clients: 20000    95per-rtt: 372ms    min-rtt: 104ms    median-rtt: 191ms    max-rtt: 577ms
clients: 21000    95per-rtt: 442ms    min-rtt: 113ms    median-rtt: 202ms    max-rtt: 556ms
clients: 22000    95per-rtt: 418ms    min-rtt: 127ms    median-rtt: 207ms    max-rtt: 551ms
clients: 23000    95per-rtt: 443ms    min-rtt: 122ms    median-rtt: 214ms    max-rtt: 587ms
clients: 24000    95per-rtt: 442ms    min-rtt: 130ms    median-rtt: 244ms    max-rtt: 550ms
clients: 25000    95per-rtt: 473ms    min-rtt: 131ms    median-rtt: 223ms    max-rtt: 733ms
```

## Java / Netty

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/java/netty(master*)% java -version                                                                                                                            {1}
java version "1.8.0_101"
Java(TM) SE Runtime Environment (build 1.8.0_101-b13)
Java HotSpot(TM) 64-Bit Server VM (build 25.101-b13, mixed mode)

dev@earth:~/hashrocket/websocket-shootout/java/netty(master*)% ../gradlew
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3031/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  33ms    min-rtt:   9ms    median-rtt:  19ms    max-rtt:  46ms
clients:  2000    95per-rtt:  58ms    min-rtt:  15ms    median-rtt:  27ms    max-rtt:  66ms
clients:  3000    95per-rtt:  78ms    min-rtt:  19ms    median-rtt:  35ms    max-rtt: 114ms
clients:  4000    95per-rtt: 166ms    min-rtt:  22ms    median-rtt:  36ms    max-rtt: 221ms
clients:  5000    95per-rtt: 201ms    min-rtt:  21ms    median-rtt:  47ms    max-rtt: 283ms
clients:  6000    95per-rtt: 158ms    min-rtt:  24ms    median-rtt:  50ms    max-rtt: 234ms
clients:  7000    95per-rtt: 242ms    min-rtt:  25ms    median-rtt:  67ms    max-rtt: 460ms
clients:  8000    95per-rtt: 221ms    min-rtt:  23ms    median-rtt:  65ms    max-rtt: 379ms
clients:  9000    95per-rtt: 256ms    min-rtt:  25ms    median-rtt:  75ms    max-rtt: 377ms
clients: 10000    95per-rtt: 322ms    min-rtt:  32ms    median-rtt:  79ms    max-rtt: 487ms
clients: 11000    95per-rtt: 411ms    min-rtt:  36ms    median-rtt: 112ms    max-rtt: 664ms
clients: 12000    95per-rtt: 298ms    min-rtt:  35ms    median-rtt:  93ms    max-rtt: 637ms
clients: 13000    95per-rtt: 438ms    min-rtt:  39ms    median-rtt:  98ms    max-rtt: 799ms
clients: 14000    95per-rtt: 377ms    min-rtt:  37ms    median-rtt: 118ms    max-rtt: 521ms
clients: 15000    95per-rtt: 441ms    min-rtt:  49ms    median-rtt: 135ms    max-rtt: 871ms
clients: 16000    95per-rtt: 468ms    min-rtt:  45ms    median-rtt: 140ms    max-rtt: 810ms
clients: 17000    95per-rtt: 441ms    min-rtt:  49ms    median-rtt: 158ms    max-rtt: 547ms
clients: 18000    95per-rtt: 482ms    min-rtt:  50ms    median-rtt: 156ms    max-rtt: 824ms
clients: 19000    95per-rtt: 486ms    min-rtt:  45ms    median-rtt: 168ms    max-rtt: 706ms
clients: 20000    95per-rtt: 499ms    min-rtt:  56ms    median-rtt: 173ms    max-rtt: 1062ms
```

## Java / Undertow

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/java/undertow(master*)% java -version                                                                                                                            {1}
java version "1.8.0_101"
Java(TM) SE Runtime Environment (build 1.8.0_101-b13)
Java HotSpot(TM) 64-Bit Server VM (build 25.101-b13, mixed mode)

dev@earth:~/hashrocket/websocket-shootout/java/undertow(master*)% ../gradlew
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3030/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  20ms    min-rtt:   5ms    median-rtt:  11ms    max-rtt:  29ms
clients:  2000    95per-rtt:  55ms    min-rtt:  10ms    median-rtt:  16ms    max-rtt: 155ms
clients:  3000    95per-rtt: 138ms    min-rtt:  13ms    median-rtt:  26ms    max-rtt: 213ms
clients:  4000    95per-rtt: 174ms    min-rtt:  15ms    median-rtt:  28ms    max-rtt: 220ms
clients:  5000    95per-rtt: 179ms    min-rtt:  18ms    median-rtt:  43ms    max-rtt: 223ms
clients:  6000    95per-rtt: 229ms    min-rtt:  21ms    median-rtt:  51ms    max-rtt: 493ms
clients:  7000    95per-rtt: 251ms    min-rtt:  24ms    median-rtt:  81ms    max-rtt: 436ms
clients:  8000    95per-rtt: 258ms    min-rtt:  24ms    median-rtt:  60ms    max-rtt: 287ms
clients:  9000    95per-rtt: 255ms    min-rtt:  28ms    median-rtt:  71ms    max-rtt: 393ms
clients: 10000    95per-rtt: 268ms    min-rtt:  31ms    median-rtt:  85ms    max-rtt: 455ms
clients: 11000    95per-rtt: 324ms    min-rtt:  29ms    median-rtt:  92ms    max-rtt: 502ms
clients: 12000    95per-rtt: 343ms    min-rtt:  36ms    median-rtt: 129ms    max-rtt: 529ms
clients: 13000    95per-rtt: 407ms    min-rtt:  38ms    median-rtt: 106ms    max-rtt: 654ms
clients: 14000    95per-rtt: 438ms    min-rtt:  39ms    median-rtt: 160ms    max-rtt: 981ms
clients: 15000    95per-rtt: 372ms    min-rtt:  43ms    median-rtt: 109ms    max-rtt: 742ms
clients: 16000    95per-rtt: 430ms    min-rtt:  44ms    median-rtt: 161ms    max-rtt: 1310ms
clients: 17000    95per-rtt: 468ms    min-rtt:  49ms    median-rtt: 137ms    max-rtt: 1021ms
clients: 18000    95per-rtt: 475ms    min-rtt:  51ms    median-rtt: 164ms    max-rtt: 766ms
clients: 19000    95per-rtt: 490ms    min-rtt:  52ms    median-rtt: 176ms    max-rtt: 805ms
```

## Javascript / Node / Faye

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/js/faye(master*)% node --version                                                                                                                            {130}
v6.6.0
dev@earth:~/hashrocket/websocket-shootout/js/faye(master*)% node index.js
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  55ms    min-rtt:  26ms    median-rtt:  38ms    max-rtt:  68ms
clients:  2000    95per-rtt:  94ms    min-rtt:  22ms    median-rtt:  79ms    max-rtt: 101ms
clients:  3000    95per-rtt: 128ms    min-rtt:  42ms    median-rtt: 120ms    max-rtt: 144ms
clients:  4000    95per-rtt: 165ms    min-rtt:  53ms    median-rtt: 156ms    max-rtt: 180ms
clients:  5000    95per-rtt: 146ms    min-rtt:  46ms    median-rtt: 142ms    max-rtt: 156ms
clients:  6000    95per-rtt: 278ms    min-rtt:  80ms    median-rtt: 235ms    max-rtt: 304ms
clients:  7000    95per-rtt: 278ms    min-rtt:  81ms    median-rtt: 274ms    max-rtt: 304ms
clients:  8000    95per-rtt: 373ms    min-rtt:  97ms    median-rtt: 315ms    max-rtt: 391ms
clients:  9000    95per-rtt: 360ms    min-rtt: 101ms    median-rtt: 355ms    max-rtt: 374ms
clients: 10000    95per-rtt: 400ms    min-rtt: 125ms    median-rtt: 395ms    max-rtt: 426ms
clients: 11000    95per-rtt: 473ms    min-rtt: 152ms    median-rtt: 434ms    max-rtt: 563ms
clients: 12000    95per-rtt: 480ms    min-rtt: 141ms    median-rtt: 475ms    max-rtt: 495ms
clients: 13000    95per-rtt: 399ms    min-rtt: 114ms    median-rtt: 379ms    max-rtt: 424ms
```

## Javascript / Node / Faye - clustered

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/js(master*)% node -v
v6.6.0
dev@earth:~/hashrocket/websocket-shootout/js(master*)% node run-cluster.js faye
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  66ms    min-rtt:   2ms    median-rtt:   8ms    max-rtt: 101ms
clients:  2000    95per-rtt:  86ms    min-rtt:   4ms    median-rtt:  13ms    max-rtt: 100ms
clients:  3000    95per-rtt: 150ms    min-rtt:   9ms    median-rtt:  20ms    max-rtt: 315ms
clients:  4000    95per-rtt: 237ms    min-rtt:  11ms    median-rtt:  18ms    max-rtt: 310ms
clients:  5000    95per-rtt: 195ms    min-rtt:  13ms    median-rtt:  29ms    max-rtt: 228ms
clients:  6000    95per-rtt: 204ms    min-rtt:  13ms    median-rtt:  42ms    max-rtt: 411ms
clients:  7000    95per-rtt: 228ms    min-rtt:  15ms    median-rtt:  41ms    max-rtt: 474ms
clients:  8000    95per-rtt: 227ms    min-rtt:  17ms    median-rtt:  63ms    max-rtt: 303ms
clients:  9000    95per-rtt: 222ms    min-rtt:  19ms    median-rtt:  79ms    max-rtt: 378ms
clients: 10000    95per-rtt: 366ms    min-rtt:  21ms    median-rtt:  64ms    max-rtt: 481ms
clients: 11000    95per-rtt: 306ms    min-rtt:  17ms    median-rtt:  86ms    max-rtt: 469ms
clients: 12000    95per-rtt: 332ms    min-rtt:  15ms    median-rtt: 106ms    max-rtt: 687ms
clients: 13000    95per-rtt: 367ms    min-rtt:  20ms    median-rtt: 116ms    max-rtt: 481ms
clients: 14000    95per-rtt: 390ms    min-rtt:  28ms    median-rtt: 100ms    max-rtt: 526ms
clients: 15000    95per-rtt: 396ms    min-rtt:  31ms    median-rtt: 114ms    max-rtt: 878ms
clients: 16000    95per-rtt: 450ms    min-rtt:  32ms    median-rtt: 113ms    max-rtt: 1046ms
clients: 17000    95per-rtt: 423ms    min-rtt:  31ms    median-rtt: 123ms    max-rtt: 491ms
clients: 18000    95per-rtt: 464ms    min-rtt:  32ms    median-rtt: 144ms    max-rtt: 667ms
clients: 19000    95per-rtt: 460ms    min-rtt:  36ms    median-rtt: 139ms    max-rtt: 1239ms
```


## Javascript / Node / ws

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/js/ws(master*)% node -v
v6.6.0
dev@earth:~/hashrocket/websocket-shootout/js/ws(master*)% node index.js
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  47ms    min-rtt:   9ms    median-rtt:  30ms    max-rtt:  53ms
clients:  2000    95per-rtt:  84ms    min-rtt:  17ms    median-rtt:  64ms    max-rtt:  88ms
clients:  3000    95per-rtt: 107ms    min-rtt:  25ms    median-rtt:  99ms    max-rtt: 115ms
clients:  4000    95per-rtt: 161ms    min-rtt:  39ms    median-rtt: 133ms    max-rtt: 177ms
clients:  5000    95per-rtt: 172ms    min-rtt:  56ms    median-rtt: 166ms    max-rtt: 196ms
clients:  6000    95per-rtt: 238ms    min-rtt:  71ms    median-rtt: 203ms    max-rtt: 249ms
clients:  7000    95per-rtt: 280ms    min-rtt:  73ms    median-rtt: 237ms    max-rtt: 297ms
clients:  8000    95per-rtt: 278ms    min-rtt:  81ms    median-rtt: 271ms    max-rtt: 293ms
clients:  9000    95per-rtt: 231ms    min-rtt:  67ms    median-rtt: 222ms    max-rtt: 246ms
clients: 10000    95per-rtt: 409ms    min-rtt: 111ms    median-rtt: 346ms    max-rtt: 437ms
clients: 11000    95per-rtt: 390ms    min-rtt: 108ms    median-rtt: 380ms    max-rtt: 406ms
clients: 12000    95per-rtt: 495ms    min-rtt: 119ms    median-rtt: 422ms    max-rtt: 540ms
clients: 13000    95per-rtt: 459ms    min-rtt: 122ms    median-rtt: 457ms    max-rtt: 466ms
clients: 14000    95per-rtt: 498ms    min-rtt: 149ms    median-rtt: 491ms    max-rtt: 525ms
```

## Javascript / Node / ws - clustered

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/js(master*)% node -v
v6.6.0
dev@earth:~/hashrocket/websocket-shootout/js(master*)% node run-cluster.js ws
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  49ms    min-rtt:   2ms    median-rtt:   8ms    max-rtt:  88ms
clients:  2000    95per-rtt:  69ms    min-rtt:   7ms    median-rtt:  14ms    max-rtt: 234ms
clients:  3000    95per-rtt: 149ms    min-rtt:   8ms    median-rtt:  21ms    max-rtt: 215ms
clients:  4000    95per-rtt: 220ms    min-rtt:  11ms    median-rtt:  23ms    max-rtt: 254ms
clients:  5000    95per-rtt: 218ms    min-rtt:  11ms    median-rtt:  33ms    max-rtt: 338ms
clients:  6000    95per-rtt: 213ms    min-rtt:  14ms    median-rtt:  37ms    max-rtt: 509ms
clients:  7000    95per-rtt: 236ms    min-rtt:   9ms    median-rtt:  46ms    max-rtt: 338ms
clients:  8000    95per-rtt: 246ms    min-rtt:  14ms    median-rtt:  60ms    max-rtt: 283ms
clients:  9000    95per-rtt: 368ms    min-rtt:  19ms    median-rtt:  58ms    max-rtt: 816ms
clients: 10000    95per-rtt: 323ms    min-rtt:  19ms    median-rtt:  85ms    max-rtt: 489ms
clients: 11000    95per-rtt: 282ms    min-rtt:  17ms    median-rtt: 120ms    max-rtt: 589ms
clients: 12000    95per-rtt: 270ms    min-rtt:  12ms    median-rtt: 112ms    max-rtt: 433ms
clients: 13000    95per-rtt: 414ms    min-rtt:  19ms    median-rtt:  89ms    max-rtt: 967ms
clients: 14000    95per-rtt: 416ms    min-rtt:  21ms    median-rtt: 123ms    max-rtt: 794ms
clients: 15000    95per-rtt: 467ms    min-rtt:  22ms    median-rtt: 126ms    max-rtt: 690ms
clients: 16000    95per-rtt: 466ms    min-rtt:  23ms    median-rtt: 132ms    max-rtt: 653ms
clients: 17000    95per-rtt: 383ms    min-rtt:  28ms    median-rtt: 165ms    max-rtt: 1112ms
```

## Javascript / Node / uws

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/js/uws(master*)% node -v
v6.6.0
dev@earth:~/hashrocket/websocket-shootout/js/uws(master*)% node index.js
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-siz
e 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  16ms    min-rtt:   5ms    median-rtt:   9ms    max-rtt:  18ms
clients:  2000    95per-rtt: 201ms    min-rtt:   8ms    median-rtt:  26ms    max-rtt: 214ms
clients:  3000    95per-rtt:  45ms    min-rtt:  12ms    median-rtt:  42ms    max-rtt:  59ms
clients:  4000    95per-rtt: 148ms    min-rtt:  10ms    median-rtt:  37ms    max-rtt: 179ms
clients:  5000    95per-rtt:  78ms    min-rtt:  25ms    median-rtt:  75ms    max-rtt:  81ms
clients:  6000    95per-rtt:  96ms    min-rtt:  22ms    median-rtt:  90ms    max-rtt: 216ms
clients:  7000    95per-rtt: 111ms    min-rtt:  28ms    median-rtt: 105ms    max-rtt: 114ms
clients:  8000    95per-rtt: 125ms    min-rtt:  32ms    median-rtt: 120ms    max-rtt: 132ms
clients:  9000    95per-rtt: 141ms    min-rtt:  37ms    median-rtt: 135ms    max-rtt: 145ms
clients: 10000    95per-rtt: 165ms    min-rtt:  32ms    median-rtt: 149ms    max-rtt: 431ms
clients: 11000    95per-rtt: 174ms    min-rtt:  44ms    median-rtt: 165ms    max-rtt: 181ms
clients: 12000    95per-rtt: 187ms    min-rtt:  47ms    median-rtt: 180ms    max-rtt: 197ms
clients: 13000    95per-rtt: 298ms    min-rtt:  37ms    median-rtt: 148ms    max-rtt: 442ms
clients: 14000    95per-rtt: 219ms    min-rtt:  56ms    median-rtt: 210ms    max-rtt: 230ms
clients: 15000    95per-rtt: 236ms    min-rtt:  74ms    median-rtt: 225ms    max-rtt: 253ms
clients: 16000    95per-rtt: 254ms    min-rtt:  63ms    median-rtt: 241ms    max-rtt: 268ms
clients: 17000    95per-rtt: 264ms    min-rtt:  67ms    median-rtt: 255ms    max-rtt: 279ms
clients: 18000    95per-rtt: 296ms    min-rtt:  71ms    median-rtt: 270ms    max-rtt: 339ms
clients: 19000    95per-rtt: 300ms    min-rtt:  61ms    median-rtt: 284ms    max-rtt: 408ms
clients: 20000    95per-rtt: 315ms    min-rtt:  80ms    median-rtt: 301ms    max-rtt: 341ms
clients: 21000    95per-rtt: 334ms    min-rtt:  85ms    median-rtt: 316ms    max-rtt: 348ms
clients: 22000    95per-rtt: 353ms    min-rtt:  64ms    median-rtt: 316ms    max-rtt: 1235ms
clients: 23000    95per-rtt: 405ms    min-rtt:  71ms    median-rtt: 342ms    max-rtt: 1003ms
clients: 24000    95per-rtt: 376ms    min-rtt:  97ms    median-rtt: 361ms    max-rtt: 395ms
clients: 25000    95per-rtt: 389ms    min-rtt:  98ms    median-rtt: 376ms    max-rtt: 445ms
clients: 26000    95per-rtt: 419ms    min-rtt:  92ms    median-rtt: 388ms    max-rtt: 1451ms
```

* Note - was only able to get near these results once.

## Javascript / Node / uws - clustered

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/js(master*)% node -v
v6.6.0
dev@earth:~/hashrocket/websocket-shootout/js(master*)% node run-cluster.js uws
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  21ms    min-rtt:   3ms    median-rtt:   8ms    max-rtt: 117ms
clients:  2000    95per-rtt: 201ms    min-rtt:   5ms    median-rtt:  13ms    max-rtt: 366ms
clients:  3000    95per-rtt: 210ms    min-rtt:   6ms    median-rtt:  37ms    max-rtt: 422ms
clients:  4000    95per-rtt: 217ms    min-rtt:   7ms    median-rtt:  29ms    max-rtt: 452ms
clients:  5000    95per-rtt: 219ms    min-rtt:   7ms    median-rtt:  53ms    max-rtt: 629ms
clients:  6000    95per-rtt: 329ms    min-rtt:   6ms    median-rtt:  59ms    max-rtt: 433ms
clients:  7000    95per-rtt: 319ms    min-rtt:   9ms    median-rtt:  70ms    max-rtt: 614ms
clients:  8000    95per-rtt: 248ms    min-rtt:   7ms    median-rtt:  76ms    max-rtt: 640ms
clients:  9000    95per-rtt: 432ms    min-rtt:  10ms    median-rtt: 154ms    max-rtt: 648ms
clients: 10000    95per-rtt: 428ms    min-rtt:   7ms    median-rtt: 145ms    max-rtt: 889ms
clients: 11000    95per-rtt: 423ms    min-rtt:  10ms    median-rtt: 101ms    max-rtt: 651ms
clients: 12000    95per-rtt: 425ms    min-rtt:   8ms    median-rtt: 131ms    max-rtt: 660ms
```

## Rails / MRI

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/ruby/action-cable-server(master*)% ruby -v
ruby 2.3.1p112 (2016-04-26 revision 54768) [x86_64-linux]
dev@earth:~/hashrocket/websocket-shootout/ruby/action-cable-server(master*)% RAILS_ENV=production SECRET_KEY_BASE=abc rails s
=> Booting Puma
=> Rails 5.0.0.1 application starting in production on http://0.0.0.0:3000
=> Run `rails server -h` for more startup options
Puma starting in single mode...
* Version 3.5.2 (ruby 2.3.1-p112), codename: Amateur Raccoon Rocketry
* Min threads: 5, max threads: 5
* Environment: production
* Listening on tcp://0.0.0.0:3000
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:3000/cable -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 100 --step-size 100 --origin http://earth.local/ --server-type actioncable
clients:   100    95per-rtt: 107ms    min-rtt:  38ms    median-rtt:  79ms    max-rtt: 120ms
clients:   200    95per-rtt: 197ms    min-rtt:  72ms    median-rtt: 158ms    max-rtt: 229ms
clients:   300    95per-rtt: 273ms    min-rtt: 141ms    median-rtt: 237ms    max-rtt: 396ms
clients:   400    95per-rtt: 405ms    min-rtt: 122ms    median-rtt: 316ms    max-rtt: 497ms
clients:   500    95per-rtt: 484ms    min-rtt: 232ms    median-rtt: 392ms    max-rtt: 557ms
2016/10/15 16:27:47 Missing received broadcasts: expected 210000, got 209991
```

## Ruby / Rails / JRuby

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/ruby/action-cable-server(master*)% ruby -v
jruby 9.1.2.0 (2.3.0) 2016-05-26 7357c8f Java HotSpot(TM) 64-Bit Server VM 25.101-b13 on 1.8.0_101-b13 +jit [linux-x86_64]
dev@earth:~/hashrocket/websocket-shootout/ruby/action-cable-server(master*)% RAILS_ENV=production SECRET_KEY_BASE=abc rails s
=> Booting Puma
=> Rails 5.0.0.1 application starting in production on http://0.0.0.0:3000
=> Run `rails server -h` for more startup options
Puma starting in single mode...
* Version 3.5.2 (jruby 9.1.2.0 - ruby 2.3.0), codename: Amateur Raccoon Rocketry
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:3000/cable -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 100 --step-size 100 --origin http://earth.local/ --server-type actioncable
clients:   100    95per-rtt: 283ms    min-rtt:  48ms    median-rtt: 124ms    max-rtt: 687ms
clients:   200    95per-rtt: 135ms    min-rtt:  33ms    median-rtt:  96ms    max-rtt: 145ms
clients:   300    95per-rtt: 140ms    min-rtt:  51ms    median-rtt: 128ms    max-rtt: 148ms
clients:   400    95per-rtt: 185ms    min-rtt:  44ms    median-rtt: 168ms    max-rtt: 186ms
clients:   500    95per-rtt: 240ms    min-rtt:  69ms    median-rtt: 212ms    max-rtt: 255ms
clients:   600    95per-rtt: 255ms    min-rtt:  74ms    median-rtt: 248ms    max-rtt: 260ms
clients:   700    95per-rtt: 298ms    min-rtt:  83ms    median-rtt: 289ms    max-rtt: 301ms
clients:   800    95per-rtt: 362ms    min-rtt:  87ms    median-rtt: 341ms    max-rtt: 374ms
clients:   900    95per-rtt: 407ms    min-rtt:  98ms    median-rtt: 375ms    max-rtt: 412ms
clients:  1000    95per-rtt: 454ms    min-rtt: 116ms    median-rtt: 424ms    max-rtt: 458ms
2016/10/15 16:24:28 Missing received broadcasts: expected 660000, got 659987
```

## Ruby / Eventmachine / MRI

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/ruby/event-machine(master*)% ruby -v                                                                                                                        {130}
ruby 2.3.1p112 (2016-04-26 revision 54768) [x86_64-linux]
dev@earth:~/hashrocket/websocket-shootout/ruby/event-machine(master*)% bundle exec ruby server.rb
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:8080/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  14ms    min-rtt:   7ms    median-rtt:  13ms    max-rtt:  20ms
clients:  2000    95per-rtt:  34ms    min-rtt:  24ms    median-rtt:  31ms    max-rtt:  39ms
clients:  3000    95per-rtt:  67ms    min-rtt:  21ms    median-rtt:  61ms    max-rtt:  74ms
clients:  4000    95per-rtt: 145ms    min-rtt:  16ms    median-rtt:  69ms    max-rtt: 299ms
clients:  5000    95per-rtt: 182ms    min-rtt:  35ms    median-rtt:  92ms    max-rtt: 253ms
clients:  6000    95per-rtt: 227ms    min-rtt:  24ms    median-rtt: 121ms    max-rtt: 292ms
clients:  7000    95per-rtt: 234ms    min-rtt:  28ms    median-rtt: 145ms    max-rtt: 295ms
clients:  8000    95per-rtt: 276ms    min-rtt:  50ms    median-rtt: 149ms    max-rtt: 314ms
clients:  9000    95per-rtt: 269ms    min-rtt:  37ms    median-rtt: 174ms    max-rtt: 353ms
clients: 10000    95per-rtt: 337ms    min-rtt:  53ms    median-rtt: 207ms    max-rtt: 393ms
clients: 11000    95per-rtt: 386ms    min-rtt:  46ms    median-rtt: 236ms    max-rtt: 457ms
clients: 12000    95per-rtt: 409ms    min-rtt:  54ms    median-rtt: 215ms    max-rtt: 461ms
clients: 13000    95per-rtt: 448ms    min-rtt:  71ms    median-rtt: 238ms    max-rtt: 592ms
```

* Note: only able to get these results on initial boot of server. The best subsequent runs reached 5000 clients.

## Ruby / Eventmachine / JRuby

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout/ruby/event-machine(master*)% ruby -v
jruby 9.1.2.0 (2.3.0) 2016-05-26 7357c8f Java HotSpot(TM) 64-Bit Server VM 25.101-b13 on 1.8.0_101-b13 +jit [linux-x86_64]
dev@earth:~/hashrocket/websocket-shootout/ruby/event-machine(master*)% bundle exec ruby server.rb
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:8080/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  67ms    min-rtt:   6ms    median-rtt:  22ms    max-rtt: 138ms
clients:  2000    95per-rtt:  55ms    min-rtt:  10ms    median-rtt:  40ms    max-rtt:  59ms
clients:  3000    95per-rtt:  75ms    min-rtt:  13ms    median-rtt:  51ms    max-rtt: 110ms
clients:  4000    95per-rtt: 105ms    min-rtt:  21ms    median-rtt:  65ms    max-rtt: 216ms
clients:  5000    95per-rtt: 161ms    min-rtt:  17ms    median-rtt:  79ms    max-rtt: 280ms
clients:  6000    95per-rtt: 183ms    min-rtt:  32ms    median-rtt: 103ms    max-rtt: 300ms
clients:  7000    95per-rtt: 246ms    min-rtt:  21ms    median-rtt: 114ms    max-rtt: 338ms
clients:  8000    95per-rtt: 268ms    min-rtt:  21ms    median-rtt: 111ms    max-rtt: 383ms
clients:  9000    95per-rtt: 345ms    min-rtt:  43ms    median-rtt: 137ms    max-rtt: 399ms
clients: 10000    95per-rtt: 345ms    min-rtt:  32ms    median-rtt: 161ms    max-rtt: 427ms
clients: 11000    95per-rtt: 326ms    min-rtt:  43ms    median-rtt: 172ms    max-rtt: 375ms
clients: 12000    95per-rtt: 360ms    min-rtt:  44ms    median-rtt: 162ms    max-rtt: 439ms
clients: 13000    95per-rtt: 384ms    min-rtt:  48ms    median-rtt: 182ms    max-rtt: 503ms
clients: 14000    95per-rtt: 398ms    min-rtt:  38ms    median-rtt: 201ms    max-rtt: 562ms
clients: 15000    95per-rtt: 429ms    min-rtt:  49ms    median-rtt: 205ms    max-rtt: 734ms
clients: 16000    95per-rtt: 497ms    min-rtt:  91ms    median-rtt: 226ms    max-rtt: 734ms
clients: 17000    95per-rtt: 459ms    min-rtt:  56ms    median-rtt: 254ms    max-rtt: 538ms
```

## Rust - ws

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% rustc --version
rustc 1.11.0 (9b21dcd6a 2016-08-15)
dev@earth:~/hashrocket/websocket-shootout(master*)% bin/rust-ws-server -a 0.0.0.0 -p 3334 ws
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  41ms    min-rtt:   2ms    median-rtt:  14ms    max-rtt:  43ms
clients:  2000    95per-rtt:  44ms    min-rtt:   8ms    median-rtt:  22ms    max-rtt:  50ms
clients:  3000    95per-rtt:  56ms    min-rtt:   9ms    median-rtt:  41ms    max-rtt:  64ms
clients:  4000    95per-rtt:  79ms    min-rtt:   6ms    median-rtt:  50ms    max-rtt:  96ms
clients:  5000    95per-rtt: 253ms    min-rtt:  10ms    median-rtt:  60ms    max-rtt: 274ms
clients:  6000    95per-rtt: 141ms    min-rtt:  10ms    median-rtt:  72ms    max-rtt: 215ms
clients:  7000    95per-rtt: 278ms    min-rtt:  11ms    median-rtt:  81ms    max-rtt: 292ms
clients:  8000    95per-rtt: 240ms    min-rtt:  22ms    median-rtt:  97ms    max-rtt: 281ms
clients:  9000    95per-rtt: 310ms    min-rtt:  16ms    median-rtt:  98ms    max-rtt: 507ms
clients: 10000    95per-rtt: 290ms    min-rtt:  33ms    median-rtt: 102ms    max-rtt: 502ms
clients: 11000    95per-rtt: 320ms    min-rtt:  21ms    median-rtt: 122ms    max-rtt: 457ms
clients: 12000    95per-rtt: 352ms    min-rtt:  23ms    median-rtt: 134ms    max-rtt: 667ms
clients: 13000    95per-rtt: 335ms    min-rtt:  20ms    median-rtt: 144ms    max-rtt: 566ms
clients: 14000    95per-rtt: 390ms    min-rtt:  39ms    median-rtt: 160ms    max-rtt: 525ms
clients: 15000    95per-rtt: 390ms    min-rtt:  26ms    median-rtt: 165ms    max-rtt: 620ms
clients: 16000    95per-rtt: 415ms    min-rtt:  31ms    median-rtt: 180ms    max-rtt: 596ms
clients: 17000    95per-rtt: 407ms    min-rtt:  22ms    median-rtt: 181ms    max-rtt: 806ms
clients: 18000    95per-rtt: 400ms    min-rtt:  30ms    median-rtt: 218ms    max-rtt: 650ms
clients: 19000    95per-rtt: 465ms    min-rtt:  30ms    median-rtt: 225ms    max-rtt: 746ms
clients: 20000    95per-rtt: 492ms    min-rtt:  38ms    median-rtt: 207ms    max-rtt: 747ms
```

## Rust - threadpool-ws

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% rustc --version
rustc 1.11.0 (9b21dcd6a 2016-08-15)
dev@earth:~/hashrocket/websocket-shootout(master*)% bin/rust-ws-server -a 0.0.0.0 -p 3334 threadpool-ws
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  37ms    min-rtt:   3ms    median-rtt:  13ms    max-rtt:  46ms
clients:  2000    95per-rtt:  47ms    min-rtt:   6ms    median-rtt:  27ms    max-rtt:  50ms
clients:  3000    95per-rtt:  68ms    min-rtt:   5ms    median-rtt:  38ms    max-rtt: 113ms
clients:  4000    95per-rtt:  92ms    min-rtt:  10ms    median-rtt:  49ms    max-rtt: 248ms
clients:  5000    95per-rtt:  94ms    min-rtt:  12ms    median-rtt:  62ms    max-rtt: 191ms
clients:  6000    95per-rtt: 181ms    min-rtt:  12ms    median-rtt:  71ms    max-rtt: 252ms
clients:  7000    95per-rtt: 248ms    min-rtt:  23ms    median-rtt:  78ms    max-rtt: 284ms
clients:  8000    95per-rtt: 286ms    min-rtt:  19ms    median-rtt:  88ms    max-rtt: 317ms
clients:  9000    95per-rtt: 288ms    min-rtt:  17ms    median-rtt: 106ms    max-rtt: 319ms
clients: 10000    95per-rtt: 270ms    min-rtt:  28ms    median-rtt: 106ms    max-rtt: 339ms
clients: 11000    95per-rtt: 322ms    min-rtt:  18ms    median-rtt: 125ms    max-rtt: 549ms
clients: 12000    95per-rtt: 320ms    min-rtt:  18ms    median-rtt: 141ms    max-rtt: 381ms
clients: 13000    95per-rtt: 361ms    min-rtt:  33ms    median-rtt: 160ms    max-rtt: 976ms
clients: 14000    95per-rtt: 342ms    min-rtt:  29ms    median-rtt: 162ms    max-rtt: 498ms
clients: 15000    95per-rtt: 414ms    min-rtt:  37ms    median-rtt: 177ms    max-rtt: 787ms
clients: 16000    95per-rtt: 444ms    min-rtt:  41ms    median-rtt: 180ms    max-rtt: 667ms
clients: 17000    95per-rtt: 440ms    min-rtt:  18ms    median-rtt: 207ms    max-rtt: 591ms
```

## Rust - scopedpool-ws

Server Environment

```
dev@earth:~/hashrocket/websocket-shootout(master*)% rustc --version
rustc 1.11.0 (9b21dcd6a 2016-08-15)
dev@earth:~/hashrocket/websocket-shootout(master*)% bin/rust-ws-server -a 0.0.0.0 -p 3334 scopedpool-ws
```

Benchmark Output

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-size 1000 --payload-padding 200 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  38ms    min-rtt:   3ms    median-rtt:  12ms    max-rtt:  42ms
clients:  2000    95per-rtt:  45ms    min-rtt:   6ms    median-rtt:  23ms    max-rtt:  49ms
clients:  3000    95per-rtt:  58ms    min-rtt:   9ms    median-rtt:  40ms    max-rtt:  79ms
clients:  4000    95per-rtt:  75ms    min-rtt:  11ms    median-rtt:  49ms    max-rtt: 101ms
clients:  5000    95per-rtt: 108ms    min-rtt:  16ms    median-rtt:  61ms    max-rtt: 173ms
clients:  6000    95per-rtt: 204ms    min-rtt:  23ms    median-rtt:  76ms    max-rtt: 239ms
clients:  7000    95per-rtt: 226ms    min-rtt:  18ms    median-rtt:  78ms    max-rtt: 308ms
clients:  8000    95per-rtt: 271ms    min-rtt:  33ms    median-rtt:  93ms    max-rtt: 314ms
clients:  9000    95per-rtt: 271ms    min-rtt:  18ms    median-rtt: 112ms    max-rtt: 316ms
clients: 10000    95per-rtt: 314ms    min-rtt:  42ms    median-rtt: 113ms    max-rtt: 469ms
clients: 11000    95per-rtt: 323ms    min-rtt:  24ms    median-rtt: 126ms    max-rtt: 340ms
clients: 12000    95per-rtt: 399ms    min-rtt:  28ms    median-rtt: 138ms    max-rtt: 712ms
clients: 13000    95per-rtt: 295ms    min-rtt:  25ms    median-rtt: 148ms    max-rtt: 481ms
clients: 14000    95per-rtt: 353ms    min-rtt:  24ms    median-rtt: 161ms    max-rtt: 659ms
clients: 15000    95per-rtt: 366ms    min-rtt:  38ms    median-rtt: 165ms    max-rtt: 546ms
clients: 16000    95per-rtt: 467ms    min-rtt:  28ms    median-rtt: 179ms    max-rtt: 710ms
clients: 17000    95per-rtt: 430ms    min-rtt:  47ms    median-rtt: 196ms    max-rtt: 501ms
clients: 18000    95per-rtt: 402ms    min-rtt:  51ms    median-rtt: 209ms    max-rtt: 506ms
clients: 19000    95per-rtt: 478ms    min-rtt:  54ms    median-rtt: 250ms    max-rtt: 668ms
```
