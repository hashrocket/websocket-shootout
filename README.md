# websocket-shootout

Environment setup (from project root)

```
export GOPATH=`pwd`/go
export PATH=$GOPATH/bin:$PATH
```

## Open file limits

If you try to benchmark with many connections you will probably run into OS level open file limits. Here is how to increase those limits.

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

## Development Requirements

### Ubuntu 16.04

```
sudo apt install libjsoncpp-dev libtclap-dev libwebsocketpp-dev
```

## Running Benchmarks


Run Action Cable Server

```
SECRET_KEY_BASE=REPLACEME rails s -p 3334 -e production
```

Benchmark Action Cable

```
bin/websocket-bench broadcast ws://REPLACEME:3334/cable -c 4 -s 40 --server-type actioncable --origin http://REPLACEME/ --step-size 100
```

Run Go Server

```
bin/go-websocket-server -address REPLACEME -port 3334
```

Benchmark Go Server


```
bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --step-size 1000
```

Run C++ Server

```
bin/cpp-websocket-server --address REPLACEME --port 3334
```

Benchmark C++ Server

```
bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --step-size 1000
```

## Results

Rails

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:3334/cable -c 4 -s 40 --server-type actioncable --origin http://earth.local/ --step-size 100
clients: 100  95per-rtt: 89.915295ms  min-rtt: 42.82682ms median-rtt: 72.441586ms max-rtt: 90.194458ms
clients: 200  95per-rtt: 197.755559ms min-rtt: 76.378232ms  median-rtt: 149.380799ms  max-rtt: 197.815171ms
clients: 300  95per-rtt: 305.479719ms min-rtt: 120.938445ms median-rtt: 247.129641ms  max-rtt: 373.608003ms
clients: 400  95per-rtt: 434.046476ms min-rtt: 126.895039ms median-rtt: 315.440673ms  max-rtt: 557.648956ms
clients: 500  95per-rtt: 434.529587ms min-rtt: 134.399069ms median-rtt: 354.620265ms  max-rtt: 456.082869ms
clients: 600  95per-rtt: 494.355864ms min-rtt: 226.620769ms median-rtt: 420.114634ms  max-rtt: 575.684985ms
```

C++

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 --step-size 1000
clients: 1000 95per-rtt: 44.382098ms  min-rtt: 12.900912ms  median-rtt: 31.253379ms max-rtt: 44.546755ms
clients: 2000 95per-rtt: 85.985055ms  min-rtt: 22.339958ms  median-rtt: 62.867071ms max-rtt: 87.405634ms
clients: 3000 95per-rtt: 145.712619ms min-rtt: 21.772945ms  median-rtt: 103.750608ms  max-rtt: 146.511839ms
clients: 4000 95per-rtt: 192.299272ms min-rtt: 55.609272ms  median-rtt: 140.420767ms  max-rtt: 203.653775ms
clients: 5000 95per-rtt: 243.138649ms min-rtt: 56.262443ms  median-rtt: 180.81314ms max-rtt: 245.226827ms
clients: 6000 95per-rtt: 316.115361ms min-rtt: 45.821039ms  median-rtt: 232.040573ms  max-rtt: 322.564717ms
clients: 7000 95per-rtt: 383.819491ms min-rtt: 121.576345ms median-rtt: 282.374478ms  max-rtt: 388.918181ms
clients: 8000 95per-rtt: 408.91856ms  min-rtt: 65.449111ms  median-rtt: 307.038834ms  max-rtt: 417.132626ms
clients: 9000 95per-rtt: 447.848027ms min-rtt: 102.902704ms median-rtt: 337.227507ms  max-rtt: 468.202097ms
```

Go

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 --step-size 1000
clients: 1000 95per-rtt: 30.664379ms  min-rtt: 4.110893ms median-rtt: 12.54746ms  max-rtt: 39.336076ms
clients: 2000 95per-rtt: 47.590885ms  min-rtt: 11.113939ms  median-rtt: 24.382629ms max-rtt: 54.2211ms
clients: 3000 95per-rtt: 82.636353ms  min-rtt: 12.524799ms  median-rtt: 31.991254ms max-rtt: 226.599155ms
clients: 4000 95per-rtt: 81.307903ms  min-rtt: 17.583606ms  median-rtt: 42.489867ms max-rtt: 81.975154ms
clients: 5000 95per-rtt: 100.968036ms min-rtt: 22.71692ms median-rtt: 54.093886ms max-rtt: 108.113387ms
clients: 6000 95per-rtt: 116.41069ms  min-rtt: 35.118443ms  median-rtt: 55.726031ms max-rtt: 119.307117ms
clients: 7000 95per-rtt: 163.662436ms min-rtt: 29.564001ms  median-rtt: 69.352988ms max-rtt: 209.318417ms
clients: 8000 95per-rtt: 172.772002ms min-rtt: 36.801466ms  median-rtt: 73.036089ms max-rtt: 202.148513ms
clients: 9000 95per-rtt: 177.556373ms min-rtt: 40.144784ms  median-rtt: 94.339109ms max-rtt: 210.881458ms
clients: 10000  95per-rtt: 212.419433ms min-rtt: 42.887532ms  median-rtt: 85.739085ms max-rtt: 241.083681ms
clients: 11000  95per-rtt: 223.181956ms min-rtt: 55.731169ms  median-rtt: 116.603661ms  max-rtt: 282.554934ms
clients: 12000  95per-rtt: 262.936091ms min-rtt: 50.924064ms  median-rtt: 117.314908ms  max-rtt: 267.849001ms
clients: 13000  95per-rtt: 282.22892ms  min-rtt: 52.278393ms  median-rtt: 139.050347ms  max-rtt: 361.743452ms
clients: 14000  95per-rtt: 283.528781ms min-rtt: 61.220161ms  median-rtt: 154.119477ms  max-rtt: 301.909987ms
clients: 15000  95per-rtt: 308.708679ms min-rtt: 76.657154ms  median-rtt: 145.64143ms max-rtt: 314.737429ms
clients: 16000  95per-rtt: 357.877893ms min-rtt: 69.86712ms median-rtt: 154.62281ms max-rtt: 368.976486ms
clients: 17000  95per-rtt: 326.247077ms min-rtt: 69.702684ms  median-rtt: 177.271375ms  max-rtt: 344.713392ms
clients: 18000  95per-rtt: 359.091894ms min-rtt: 78.80026ms median-rtt: 199.437404ms  max-rtt: 366.581344ms
clients: 19000  95per-rtt: 467.71395ms  min-rtt: 86.986842ms  median-rtt: 184.039978ms  max-rtt: 479.705353ms
clients: 20000  95per-rtt: 347.278097ms min-rtt: 87.937885ms  median-rtt: 189.543723ms  max-rtt: 502.16699ms
clients: 21000  95per-rtt: 408.31974ms  min-rtt: 87.804292ms  median-rtt: 200.883407ms  max-rtt: 519.165509ms
clients: 22000  95per-rtt: 381.067744ms min-rtt: 138.056942ms median-rtt: 210.187499ms  max-rtt: 450.48821ms
clients: 23000  95per-rtt: 476.026066ms min-rtt: 95.045688ms  median-rtt: 217.007338ms  max-rtt: 484.188221ms
clients: 24000  95per-rtt: 435.110799ms min-rtt: 101.119899ms median-rtt: 226.36324ms max-rtt: 563.035748ms
```

Go GOMAXPROCS=1

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 --step-size 1000
clients: 1000 95per-rtt: 41.307607ms  min-rtt: 3.67785ms  median-rtt: 13.563823ms max-rtt: 45.663905ms
clients: 2000 95per-rtt: 42.061532ms  min-rtt: 11.234271ms  median-rtt: 23.575083ms max-rtt: 42.976899ms
clients: 3000 95per-rtt: 97.820899ms  min-rtt: 12.192434ms  median-rtt: 40.436707ms max-rtt: 114.885646ms
clients: 4000 95per-rtt: 105.208681ms min-rtt: 17.127179ms  median-rtt: 55.355856ms max-rtt: 120.008658ms
clients: 5000 95per-rtt: 212.295146ms min-rtt: 20.083135ms  median-rtt: 63.883483ms max-rtt: 230.367116ms
clients: 6000 95per-rtt: 181.848525ms min-rtt: 24.333013ms  median-rtt: 80.602517ms max-rtt: 200.011528ms
clients: 7000 95per-rtt: 237.81969ms  min-rtt: 30.886387ms  median-rtt: 102.381537ms  max-rtt: 259.491088ms
clients: 8000 95per-rtt: 252.619289ms min-rtt: 31.854419ms  median-rtt: 106.750171ms  max-rtt: 350.825208ms
clients: 9000 95per-rtt: 263.537938ms min-rtt: 50.799095ms  median-rtt: 128.31274ms max-rtt: 367.463884ms
clients: 10000  95per-rtt: 354.831872ms min-rtt: 37.021212ms  median-rtt: 141.636641ms  max-rtt: 383.335423ms
clients: 11000  95per-rtt: 397.744077ms min-rtt: 47.731915ms  median-rtt: 148.590297ms  max-rtt: 414.324042ms
clients: 12000  95per-rtt: 487.371984ms min-rtt: 79.221555ms  median-rtt: 172.798211ms  max-rtt: 492.383092ms
```
