# Round 2 Extra Results

This is a sample of results from different benchmark parameters.

The below results are without any payload padding.

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
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  43ms    min-rtt:   4ms    median-rtt:   9ms    max-rtt:  44ms
clients:  2000    95per-rtt:  50ms    min-rtt:   5ms    median-rtt:  15ms    max-rtt: 206ms
clients:  3000    95per-rtt: 211ms    min-rtt:   8ms    median-rtt:  20ms    max-rtt: 212ms
clients:  4000    95per-rtt:  57ms    min-rtt:   9ms    median-rtt:  18ms    max-rtt:  83ms
clients:  5000    95per-rtt: 146ms    min-rtt:  10ms    median-rtt:  20ms    max-rtt: 204ms
clients:  6000    95per-rtt:  92ms    min-rtt:  11ms    median-rtt:  27ms    max-rtt: 188ms
clients:  7000    95per-rtt: 205ms    min-rtt:  12ms    median-rtt:  43ms    max-rtt: 220ms
clients:  8000    95per-rtt: 107ms    min-rtt:  14ms    median-rtt:  31ms    max-rtt: 114ms
clients:  9000    95per-rtt: 123ms    min-rtt:  15ms    median-rtt:  26ms    max-rtt: 134ms
clients: 10000    95per-rtt: 161ms    min-rtt:  15ms    median-rtt:  39ms    max-rtt: 199ms
clients: 11000    95per-rtt: 213ms    min-rtt:  17ms    median-rtt:  42ms    max-rtt: 301ms
clients: 12000    95per-rtt: 213ms    min-rtt:  19ms    median-rtt:  37ms    max-rtt: 285ms
clients: 13000    95per-rtt: 222ms    min-rtt:  20ms    median-rtt:  54ms    max-rtt: 231ms
clients: 14000    95per-rtt: 209ms    min-rtt:  23ms    median-rtt:  51ms    max-rtt: 275ms
clients: 15000    95per-rtt: 241ms    min-rtt:  23ms    median-rtt:  51ms    max-rtt: 244ms
clients: 16000    95per-rtt: 161ms    min-rtt:  22ms    median-rtt:  66ms    max-rtt: 179ms
clients: 17000    95per-rtt: 235ms    min-rtt:  25ms    median-rtt:  56ms    max-rtt: 238ms
clients: 18000    95per-rtt: 229ms    min-rtt:  25ms    median-rtt:  68ms    max-rtt: 265ms
clients: 19000    95per-rtt: 234ms    min-rtt:  27ms    median-rtt:  59ms    max-rtt: 305ms
clients: 20000    95per-rtt: 239ms    min-rtt:  27ms    median-rtt:  69ms    max-rtt: 248ms
clients: 21000    95per-rtt: 197ms    min-rtt:  30ms    median-rtt:  76ms    max-rtt: 220ms
clients: 22000    95per-rtt: 317ms    min-rtt:  29ms    median-rtt:  88ms    max-rtt: 320ms
clients: 23000    95per-rtt: 231ms    min-rtt:  34ms    median-rtt:  75ms    max-rtt: 251ms
clients: 24000    95per-rtt: 298ms    min-rtt:  35ms    median-rtt:  79ms    max-rtt: 360ms
clients: 25000    95per-rtt: 179ms    min-rtt:  36ms    median-rtt:  93ms    max-rtt: 228ms
clients: 26000    95per-rtt: 283ms    min-rtt:  38ms    median-rtt:  97ms    max-rtt: 498ms
clients: 27000    95per-rtt: 334ms    min-rtt:  40ms    median-rtt:  97ms    max-rtt: 342ms
clients: 28000    95per-rtt: 253ms    min-rtt:  39ms    median-rtt: 104ms    max-rtt: 291ms
clients: 29000    95per-rtt: 284ms    min-rtt:  44ms    median-rtt:  90ms    max-rtt: 609ms
clients: 30000    95per-rtt: 233ms    min-rtt:  43ms    median-rtt: 121ms    max-rtt: 264ms
clients: 31000    95per-rtt: 270ms    min-rtt:  43ms    median-rtt:  95ms    max-rtt: 518ms
clients: 32000    95per-rtt: 258ms    min-rtt:  43ms    median-rtt: 142ms    max-rtt: 272ms
clients: 33000    95per-rtt: 375ms    min-rtt:  42ms    median-rtt: 134ms    max-rtt: 655ms
clients: 34000    95per-rtt: 374ms    min-rtt:  48ms    median-rtt: 113ms    max-rtt: 451ms
clients: 35000    95per-rtt: 270ms    min-rtt:  58ms    median-rtt: 138ms    max-rtt: 396ms
clients: 36000    95per-rtt: 390ms    min-rtt:  53ms    median-rtt: 125ms    max-rtt: 626ms
clients: 37000    95per-rtt: 338ms    min-rtt:  56ms    median-rtt: 141ms    max-rtt: 458ms
clients: 38000    95per-rtt: 372ms    min-rtt:  52ms    median-rtt: 131ms    max-rtt: 415ms
clients: 39000    95per-rtt: 398ms    min-rtt:  59ms    median-rtt: 134ms    max-rtt: 447ms
clients: 40000    95per-rtt: 338ms    min-rtt:  54ms    median-rtt: 153ms    max-rtt: 554ms
clients: 41000    95per-rtt: 494ms    min-rtt:  58ms    median-rtt: 161ms    max-rtt: 536ms
clients: 42000    95per-rtt: 497ms    min-rtt:  59ms    median-rtt: 160ms    max-rtt: 588ms
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
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:3334/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-siz
e 1000 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  44ms    min-rtt:   4ms    median-rtt:   6ms    max-rtt:  46ms
clients:  2000    95per-rtt:  45ms    min-rtt:   7ms    median-rtt:  16ms    max-rtt:  50ms
clients:  3000    95per-rtt:  54ms    min-rtt:   9ms    median-rtt:  31ms    max-rtt:  60ms
clients:  4000    95per-rtt:  57ms    min-rtt:  14ms    median-rtt:  47ms    max-rtt:  65ms
clients:  5000    95per-rtt:  74ms    min-rtt:  18ms    median-rtt:  63ms    max-rtt:  82ms
clients:  6000    95per-rtt:  83ms    min-rtt:  21ms    median-rtt:  78ms    max-rtt: 110ms
clients:  7000    95per-rtt:  96ms    min-rtt:  27ms    median-rtt:  93ms    max-rtt: 102ms
clients:  8000    95per-rtt: 113ms    min-rtt:  28ms    median-rtt: 106ms    max-rtt: 119ms
clients:  9000    95per-rtt: 127ms    min-rtt:  32ms    median-rtt: 120ms    max-rtt: 143ms
clients: 10000    95per-rtt: 153ms    min-rtt:  35ms    median-rtt: 131ms    max-rtt: 229ms
clients: 11000    95per-rtt: 156ms    min-rtt:  46ms    median-rtt: 146ms    max-rtt: 168ms
clients: 12000    95per-rtt: 166ms    min-rtt:  42ms    median-rtt: 159ms    max-rtt: 173ms
clients: 13000    95per-rtt: 185ms    min-rtt:  46ms    median-rtt: 172ms    max-rtt: 187ms
clients: 14000    95per-rtt: 200ms    min-rtt:  49ms    median-rtt: 185ms    max-rtt: 208ms
clients: 15000    95per-rtt: 206ms    min-rtt:  53ms    median-rtt: 200ms    max-rtt: 221ms
clients: 16000    95per-rtt: 223ms    min-rtt:  75ms    median-rtt: 212ms    max-rtt: 243ms
clients: 17000    95per-rtt: 236ms    min-rtt:  60ms    median-rtt: 226ms    max-rtt: 251ms
clients: 18000    95per-rtt: 250ms    min-rtt:  63ms    median-rtt: 239ms    max-rtt: 300ms
clients: 19000    95per-rtt: 267ms    min-rtt:  67ms    median-rtt: 252ms    max-rtt: 303ms
clients: 20000    95per-rtt: 279ms    min-rtt:  70ms    median-rtt: 254ms    max-rtt: 319ms
clients: 21000    95per-rtt: 304ms    min-rtt:  73ms    median-rtt: 280ms    max-rtt: 350ms
clients: 22000    95per-rtt: 302ms    min-rtt:  77ms    median-rtt: 228ms    max-rtt: 351ms
clients: 23000    95per-rtt: 323ms    min-rtt:  81ms    median-rtt: 307ms    max-rtt: 339ms
clients: 24000    95per-rtt: 338ms    min-rtt:  85ms    median-rtt: 320ms    max-rtt: 352ms
clients: 25000    95per-rtt: 341ms    min-rtt:  87ms    median-rtt: 328ms    max-rtt: 400ms
clients: 26000    95per-rtt: 362ms    min-rtt:  91ms    median-rtt: 347ms    max-rtt: 435ms
clients: 27000    95per-rtt: 370ms    min-rtt:  94ms    median-rtt: 359ms    max-rtt: 382ms
clients: 28000    95per-rtt: 391ms    min-rtt:  98ms    median-rtt: 374ms    max-rtt: 448ms
clients: 29000    95per-rtt: 397ms    min-rtt:  82ms    median-rtt: 281ms    max-rtt: 420ms
clients: 30000    95per-rtt: 419ms    min-rtt: 105ms    median-rtt: 399ms    max-rtt: 447ms
clients: 31000    95per-rtt: 445ms    min-rtt:  72ms    median-rtt: 415ms    max-rtt: 462ms
clients: 32000    95per-rtt: 451ms    min-rtt: 112ms    median-rtt: 432ms    max-rtt: 473ms
clients: 33000    95per-rtt: 474ms    min-rtt:  90ms    median-rtt: 251ms    max-rtt: 647ms
clients: 34000    95per-rtt: 464ms    min-rtt:  69ms    median-rtt: 457ms    max-rtt: 501ms
clients: 35000    95per-rtt: 479ms    min-rtt: 122ms    median-rtt: 471ms    max-rtt: 493ms
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
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:3000/ -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --concurrent 4 --sample-size 100 --step-siz
e 1000 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  20ms    min-rtt:   5ms    median-rtt:  11ms    max-rtt:  28ms
clients:  2000    95per-rtt:  44ms    min-rtt:  11ms    median-rtt:  21ms    max-rtt:  65ms
clients:  3000    95per-rtt:  50ms    min-rtt:  15ms    median-rtt:  29ms    max-rtt:  56ms
clients:  4000    95per-rtt:  89ms    min-rtt:  23ms    median-rtt:  38ms    max-rtt: 115ms
clients:  5000    95per-rtt: 106ms    min-rtt:  27ms    median-rtt:  50ms    max-rtt: 158ms
clients:  6000    95per-rtt: 111ms    min-rtt:  31ms    median-rtt:  58ms    max-rtt: 225ms
clients:  7000    95per-rtt: 109ms    min-rtt:  36ms    median-rtt:  59ms    max-rtt: 208ms
clients:  8000    95per-rtt: 158ms    min-rtt:  37ms    median-rtt:  69ms    max-rtt: 329ms
clients:  9000    95per-rtt: 138ms    min-rtt:  53ms    median-rtt:  81ms    max-rtt: 234ms
clients: 10000    95per-rtt: 195ms    min-rtt:  57ms    median-rtt:  83ms    max-rtt: 360ms
clients: 11000    95per-rtt: 224ms    min-rtt:  51ms    median-rtt:  94ms    max-rtt: 343ms
clients: 12000    95per-rtt: 160ms    min-rtt:  66ms    median-rtt: 102ms    max-rtt: 210ms
clients: 13000    95per-rtt: 176ms    min-rtt:  67ms    median-rtt: 104ms    max-rtt: 307ms
clients: 14000    95per-rtt: 221ms    min-rtt:  74ms    median-rtt: 120ms    max-rtt: 420ms
clients: 15000    95per-rtt: 307ms    min-rtt:  82ms    median-rtt: 128ms    max-rtt: 421ms
clients: 16000    95per-rtt: 244ms    min-rtt:  86ms    median-rtt: 125ms    max-rtt: 354ms
clients: 17000    95per-rtt: 263ms    min-rtt:  87ms    median-rtt: 144ms    max-rtt: 432ms
clients: 18000    95per-rtt: 241ms    min-rtt:  84ms    median-rtt: 157ms    max-rtt: 475ms
clients: 19000    95per-rtt: 266ms    min-rtt: 105ms    median-rtt: 150ms    max-rtt: 395ms
clients: 20000    95per-rtt: 291ms    min-rtt: 102ms    median-rtt: 177ms    max-rtt: 461ms
clients: 21000    95per-rtt: 306ms    min-rtt: 108ms    median-rtt: 165ms    max-rtt: 513ms
clients: 22000    95per-rtt: 315ms    min-rtt: 137ms    median-rtt: 184ms    max-rtt: 458ms
clients: 23000    95per-rtt: 349ms    min-rtt: 127ms    median-rtt: 188ms    max-rtt: 391ms
clients: 24000    95per-rtt: 361ms    min-rtt: 124ms    median-rtt: 193ms    max-rtt: 547ms
clients: 25000    95per-rtt: 366ms    min-rtt: 133ms    median-rtt: 203ms    max-rtt: 630ms
clients: 26000    95per-rtt: 400ms    min-rtt: 142ms    median-rtt: 213ms    max-rtt: 576ms
clients: 27000    95per-rtt: 405ms    min-rtt: 152ms    median-rtt: 216ms    max-rtt: 582ms
clients: 28000    95per-rtt: 402ms    min-rtt: 158ms    median-rtt: 241ms    max-rtt: 612ms
clients: 29000    95per-rtt: 404ms    min-rtt: 170ms    median-rtt: 234ms    max-rtt: 667ms
clients: 30000    95per-rtt: 456ms    min-rtt: 161ms    median-rtt: 262ms    max-rtt: 583ms
clients: 31000    95per-rtt: 444ms    min-rtt: 193ms    median-rtt: 280ms    max-rtt: 502ms
clients: 32000    95per-rtt: 494ms    min-rtt: 181ms    median-rtt: 282ms    max-rtt: 576ms
clients: 33000    95per-rtt: 462ms    min-rtt: 176ms    median-rtt: 289ms    max-rtt: 536ms
clients: 34000    95per-rtt: 442ms    min-rtt: 184ms    median-rtt: 303ms    max-rtt: 512ms
clients: 35000    95per-rtt: 461ms    min-rtt: 195ms    median-rtt: 293ms    max-rtt: 653ms
clients: 36000    95per-rtt: 435ms    min-rtt: 222ms    median-rtt: 311ms    max-rtt: 506ms
clients: 37000    95per-rtt: 478ms    min-rtt: 184ms    median-rtt: 330ms    max-rtt: 674ms
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
dev@mercury:~/hashrocket/websocket-shootout(master+)% bin/websocket-bench broadcast ws://192.168.50.12:4000/ws -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -c 4 -s 40 --step-size 1000 -w neptune:4000 -w uranus:4000 -w saturn:4000 -w venus:4000
clients:  1000    95per-rtt:  14ms    min-rtt:   5ms    median-rtt:   9ms    max-rtt:  17ms
clients:  2000    95per-rtt:  30ms    min-rtt:   8ms    median-rtt:  12ms    max-rtt:  33ms
clients:  3000    95per-rtt:  38ms    min-rtt:  12ms    median-rtt:  23ms    max-rtt:  41ms
clients:  4000    95per-rtt:  78ms    min-rtt:  14ms    median-rtt:  26ms    max-rtt: 222ms
clients:  5000    95per-rtt:  82ms    min-rtt:  16ms    median-rtt:  28ms    max-rtt: 103ms
clients:  6000    95per-rtt:  66ms    min-rtt:  20ms    median-rtt:  35ms    max-rtt:  71ms
clients:  7000    95per-rtt: 122ms    min-rtt:  22ms    median-rtt:  40ms    max-rtt: 296ms
clients:  8000    95per-rtt: 118ms    min-rtt:  24ms    median-rtt:  41ms    max-rtt: 148ms
clients:  9000    95per-rtt: 173ms    min-rtt:  28ms    median-rtt:  42ms    max-rtt: 203ms
clients: 10000    95per-rtt: 124ms    min-rtt:  32ms    median-rtt:  59ms    max-rtt: 188ms
clients: 11000    95per-rtt: 151ms    min-rtt:  35ms    median-rtt:  64ms    max-rtt: 162ms
clients: 12000    95per-rtt: 249ms    min-rtt:  36ms    median-rtt:  58ms    max-rtt: 259ms
clients: 13000    95per-rtt: 232ms    min-rtt:  41ms    median-rtt:  63ms    max-rtt: 237ms
clients: 14000    95per-rtt: 287ms    min-rtt:  44ms    median-rtt:  71ms    max-rtt: 346ms
clients: 15000    95per-rtt: 199ms    min-rtt:  46ms    median-rtt:  71ms    max-rtt: 245ms
clients: 16000    95per-rtt: 193ms    min-rtt:  49ms    median-rtt:  81ms    max-rtt: 217ms
clients: 17000    95per-rtt: 254ms    min-rtt:  51ms    median-rtt:  96ms    max-rtt: 394ms
clients: 18000    95per-rtt: 293ms    min-rtt:  54ms    median-rtt: 109ms    max-rtt: 523ms
clients: 19000    95per-rtt: 264ms    min-rtt:  59ms    median-rtt:  97ms    max-rtt: 268ms
clients: 20000    95per-rtt: 303ms    min-rtt:  62ms    median-rtt: 101ms    max-rtt: 486ms
clients: 21000    95per-rtt: 229ms    min-rtt:  64ms    median-rtt: 110ms    max-rtt: 233ms
clients: 22000    95per-rtt: 257ms    min-rtt:  69ms    median-rtt: 107ms    max-rtt: 553ms
clients: 23000    95per-rtt: 289ms    min-rtt:  68ms    median-rtt: 107ms    max-rtt: 390ms
clients: 24000    95per-rtt: 285ms    min-rtt:  72ms    median-rtt: 121ms    max-rtt: 329ms
clients: 25000    95per-rtt: 311ms    min-rtt:  72ms    median-rtt: 149ms    max-rtt: 419ms
clients: 26000    95per-rtt: 395ms    min-rtt:  80ms    median-rtt: 134ms    max-rtt: 528ms
clients: 27000    95per-rtt: 319ms    min-rtt:  87ms    median-rtt: 143ms    max-rtt: 328ms
clients: 28000    95per-rtt: 374ms    min-rtt:  89ms    median-rtt: 151ms    max-rtt: 378ms
clients: 29000    95per-rtt: 424ms    min-rtt:  87ms    median-rtt: 150ms    max-rtt: 471ms
clients: 30000    95per-rtt: 441ms    min-rtt:  91ms    median-rtt: 153ms    max-rtt: 461ms
clients: 31000    95per-rtt: 418ms    min-rtt:  99ms    median-rtt: 212ms    max-rtt: 493ms
clients: 32000    95per-rtt: 336ms    min-rtt:  98ms    median-rtt: 165ms    max-rtt: 353ms
clients: 33000    95per-rtt: 404ms    min-rtt: 108ms    median-rtt: 194ms    max-rtt: 499ms
clients: 34000    95per-rtt: 322ms    min-rtt: 105ms    median-rtt: 170ms    max-rtt: 372ms
clients: 35000    95per-rtt: 433ms    min-rtt: 109ms    median-rtt: 185ms    max-rtt: 521ms
clients: 36000    95per-rtt: 347ms    min-rtt: 113ms    median-rtt: 211ms    max-rtt: 505ms
clients: 37000    95per-rtt: 362ms    min-rtt: 114ms    median-rtt: 172ms    max-rtt: 532ms
clients: 38000    95per-rtt: 370ms    min-rtt: 118ms    median-rtt: 188ms    max-rtt: 407ms
clients: 39000    95per-rtt: 455ms    min-rtt: 119ms    median-rtt: 188ms    max-rtt: 627ms
```
