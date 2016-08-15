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

These results are from running the server on one machine and the benchmark tool as another. Both machines are 4ghz i7 4790Ks running Ubuntu 16.04. Tests were run multiple times and the best results were recorded.

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
dev@mercury:~/hashrocket/websocket-shootout(master)% bin/websocket-bench broadcast ws://earth.local:3334/ws -c 4 -s 40 --step-size 1000 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -l 192.168.50.247 -l 192.168.50.248 -l 192.168.50.249 -l 192.168.50.250 -l 192.168.50.251 -l 192.168.50.252
clients: 1000 95per-rtt: 24.577204ms  min-rtt: 4.091507ms median-rtt: 12.782198ms max-rtt: 42.415756ms
clients: 2000 95per-rtt: 44.704718ms  min-rtt: 10.320552ms  median-rtt: 22.128146ms max-rtt: 47.594475ms
clients: 3000 95per-rtt: 66.196982ms  min-rtt: 12.963635ms  median-rtt: 34.579619ms max-rtt: 224.629748ms
clients: 4000 95per-rtt: 85.473984ms  min-rtt: 21.074557ms  median-rtt: 44.874809ms max-rtt: 101.500364ms
clients: 5000 95per-rtt: 98.792457ms  min-rtt: 24.583176ms  median-rtt: 51.237504ms max-rtt: 110.540583ms
clients: 6000 95per-rtt: 126.279156ms min-rtt: 27.16688ms median-rtt: 55.343828ms max-rtt: 185.79115ms
clients: 7000 95per-rtt: 137.955835ms min-rtt: 35.997018ms  median-rtt: 75.129469ms max-rtt: 159.308823ms
clients: 8000 95per-rtt: 159.672335ms min-rtt: 35.359538ms  median-rtt: 77.004178ms max-rtt: 172.739154ms
clients: 9000 95per-rtt: 219.310456ms min-rtt: 44.112143ms  median-rtt: 88.533779ms max-rtt: 245.292876ms
clients: 10000  95per-rtt: 200.434121ms min-rtt: 50.51915ms median-rtt: 101.639121ms  max-rtt: 247.71032ms
clients: 11000  95per-rtt: 206.444166ms min-rtt: 51.686545ms  median-rtt: 105.974535ms  max-rtt: 267.141568ms
clients: 12000  95per-rtt: 260.71756ms  min-rtt: 58.101275ms  median-rtt: 129.614697ms  max-rtt: 273.193258ms
clients: 13000  95per-rtt: 271.170357ms min-rtt: 51.389128ms  median-rtt: 135.344987ms  max-rtt: 319.67284ms
clients: 14000  95per-rtt: 465.530962ms min-rtt: 57.210809ms  median-rtt: 135.030609ms  max-rtt: 491.143ms
clients: 15000  95per-rtt: 340.887024ms min-rtt: 66.902937ms  median-rtt: 148.662568ms  max-rtt: 363.154677ms
clients: 16000  95per-rtt: 308.908847ms min-rtt: 72.073278ms  median-rtt: 161.76622ms max-rtt: 334.469443ms
clients: 17000  95per-rtt: 370.748573ms min-rtt: 79.032122ms  median-rtt: 186.017799ms  max-rtt: 396.473655ms
clients: 18000  95per-rtt: 326.510518ms min-rtt: 86.3994ms  median-rtt: 178.812358ms  max-rtt: 392.012241ms
clients: 19000  95per-rtt: 395.73633ms  min-rtt: 79.526544ms  median-rtt: 179.709451ms  max-rtt: 417.974979ms
clients: 20000  95per-rtt: 434.018193ms min-rtt: 85.338797ms  median-rtt: 189.017916ms  max-rtt: 435.916021ms
clients: 21000  95per-rtt: 465.659769ms min-rtt: 109.001589ms median-rtt: 216.353628ms  max-rtt: 492.108389ms
clients: 22000  95per-rtt: 463.333205ms min-rtt: 94.216702ms  median-rtt: 210.397052ms  max-rtt: 471.447606ms
clients: 23000  95per-rtt: 439.975716ms min-rtt: 97.438912ms  median-rtt: 237.085804ms  max-rtt: 509.949147ms
clients: 24000  95per-rtt: 435.028728ms min-rtt: 154.197944ms median-rtt: 241.178006ms  max-rtt: 444.462412ms
clients: 25000  95per-rtt: 482.795612ms min-rtt: 107.152358ms median-rtt: 236.115307ms  max-rtt: 591.822568ms
clients: 26000  95per-rtt: 444.560714ms min-rtt: 116.548815ms median-rtt: 245.43298ms max-rtt: 449.502996ms
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

Clojure

```
dev@mercury:~/hashrocket/websocket-shootout(master*)% bin/websocket-bench broadcast ws://earth.local:4001/ws -c 4 -s 40 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -l 192.168.50.247 -l 192.168.50.248 -l 192.168.50.249 -l 192.168.50.250 -l 192.168.50.251 -l 192.168.50.252 --step-size 1000
clients: 1000 95per-rtt: 44.479689ms  min-rtt: 9.817251ms median-rtt: 40.608106ms max-rtt: 44.945433ms
clients: 2000 95per-rtt: 53.472395ms  min-rtt: 12.223932ms  median-rtt: 44.010411ms max-rtt: 63.95257ms
clients: 3000 95per-rtt: 71.921939ms  min-rtt: 16.452917ms  median-rtt: 48.012048ms max-rtt: 72.060477ms
clients: 4000 95per-rtt: 82.824192ms  min-rtt: 26.06899ms median-rtt: 51.128476ms max-rtt: 96.470236ms
clients: 5000 95per-rtt: 90.360438ms  min-rtt: 40.953884ms  median-rtt: 57.062897ms max-rtt: 92.149554ms
clients: 6000 95per-rtt: 86.623493ms  min-rtt: 36.70016ms median-rtt: 63.350644ms max-rtt: 89.391281ms
clients: 7000 95per-rtt: 203.18797ms  min-rtt: 34.173653ms  median-rtt: 65.02101ms  max-rtt: 248.533618ms
clients: 8000 95per-rtt: 167.676516ms min-rtt: 45.918772ms  median-rtt: 72.279044ms max-rtt: 227.725781ms
clients: 9000 95per-rtt: 149.456932ms min-rtt: 38.653159ms  median-rtt: 83.845461ms max-rtt: 163.52685ms
clients: 10000  95per-rtt: 201.328911ms min-rtt: 44.057652ms  median-rtt: 90.106988ms max-rtt: 236.30086ms
clients: 11000  95per-rtt: 176.027929ms min-rtt: 66.975085ms  median-rtt: 106.118454ms  max-rtt: 201.719666ms
clients: 12000  95per-rtt: 227.832264ms min-rtt: 53.156427ms  median-rtt: 109.433128ms  max-rtt: 236.657534ms
clients: 13000  95per-rtt: 258.093436ms min-rtt: 54.702717ms  median-rtt: 120.188904ms  max-rtt: 259.961443ms
clients: 14000  95per-rtt: 247.197601ms min-rtt: 74.028831ms  median-rtt: 127.638602ms  max-rtt: 261.798302ms
clients: 15000  95per-rtt: 299.838015ms min-rtt: 62.057793ms  median-rtt: 128.891532ms  max-rtt: 403.731923ms
clients: 16000  95per-rtt: 290.620332ms min-rtt: 100.226581ms median-rtt: 142.601031ms  max-rtt: 293.933556ms
clients: 17000  95per-rtt: 319.374233ms min-rtt: 71.972634ms  median-rtt: 158.328174ms  max-rtt: 422.53312ms
clients: 18000  95per-rtt: 337.783397ms min-rtt: 81.441256ms  median-rtt: 161.652714ms  max-rtt: 371.671795ms
clients: 19000  95per-rtt: 346.358207ms min-rtt: 83.097869ms  median-rtt: 173.963224ms  max-rtt: 362.514036ms
clients: 20000  95per-rtt: 361.548183ms min-rtt: 83.615252ms  median-rtt: 190.312374ms  max-rtt: 378.754763ms
clients: 21000  95per-rtt: 365.49745ms  min-rtt: 105.159774ms median-rtt: 185.849216ms  max-rtt: 424.106263ms
clients: 22000  95per-rtt: 397.675353ms min-rtt: 115.630453ms median-rtt: 205.77204ms max-rtt: 405.019913ms
clients: 23000  95per-rtt: 444.007768ms min-rtt: 107.761792ms median-rtt: 218.239287ms  max-rtt: 473.803613ms
clients: 24000  95per-rtt: 448.694585ms min-rtt: 111.526311ms median-rtt: 220.555543ms  max-rtt: 482.355834ms
clients: 25000  95per-rtt: 467.296044ms min-rtt: 114.385484ms median-rtt: 226.830833ms  max-rtt: 525.046246ms
clients: 26000  95per-rtt: 408.390687ms min-rtt: 103.991165ms median-rtt: 228.215606ms  max-rtt: 483.160648ms
clients: 27000  95per-rtt: 462.905037ms min-rtt: 122.087508ms median-rtt: 236.354512ms  max-rtt: 552.807707ms
```

Elixir / Phoenix

```
dev@mercury:~/hashrocket/websocket-shootout(master)% bin/websocket-bench broadcast --server-type phoenix -c 4 -s 40 --step-size 1000 -l 192.168.50.5 -l 192.168.50.246 -l 192.168.50.247 -l 192.168.50.247 -l 192.168.50.248 -l 192.168.50.249 -l 192.168.50.250 -l 192.168.50.251 -l 192.168.50.252 ws://earth.local:4000/socket/websocket
clients: 1000 95per-rtt: 34.752448ms  min-rtt: 4.230879ms median-rtt: 11.014644ms max-rtt: 44.012471ms
clients: 2000 95per-rtt: 44.390532ms  min-rtt: 8.559931ms median-rtt: 23.099905ms max-rtt: 45.617566ms
clients: 3000 95per-rtt: 64.840853ms  min-rtt: 18.032724ms  median-rtt: 34.09886ms  max-rtt: 229.298322ms
clients: 4000 95per-rtt: 91.574577ms  min-rtt: 21.187067ms  median-rtt: 44.187483ms max-rtt: 112.013294ms
clients: 5000 95per-rtt: 202.724569ms min-rtt: 23.558677ms  median-rtt: 57.261323ms max-rtt: 230.207218ms
clients: 6000 95per-rtt: 120.558877ms min-rtt: 24.392554ms  median-rtt: 63.637343ms max-rtt: 123.597299ms
clients: 7000 95per-rtt: 128.122742ms min-rtt: 30.235136ms  median-rtt: 76.944281ms max-rtt: 131.761039ms
clients: 8000 95per-rtt: 206.466607ms min-rtt: 33.266766ms  median-rtt: 89.660295ms max-rtt: 215.976673ms
clients: 9000 95per-rtt: 159.782194ms min-rtt: 59.988584ms  median-rtt: 97.131684ms max-rtt: 160.993978ms
clients: 10000  95per-rtt: 182.435934ms min-rtt: 37.316316ms  median-rtt: 115.470112ms  max-rtt: 262.650196ms
clients: 11000  95per-rtt: 318.021955ms min-rtt: 58.426305ms  median-rtt: 122.063904ms  max-rtt: 327.304214ms
clients: 12000  95per-rtt: 215.336706ms min-rtt: 74.740085ms  median-rtt: 141.830783ms  max-rtt: 249.346293ms
clients: 13000  95per-rtt: 324.07202ms  min-rtt: 60.955297ms  median-rtt: 141.253741ms  max-rtt: 326.220278ms
clients: 14000  95per-rtt: 313.495446ms min-rtt: 68.052912ms  median-rtt: 157.188547ms  max-rtt: 326.799056ms
clients: 15000  95per-rtt: 263.686611ms min-rtt: 101.873994ms median-rtt: 171.879154ms  max-rtt: 291.983965ms
clients: 16000  95per-rtt: 336.697394ms min-rtt: 73.777169ms  median-rtt: 201.190146ms  max-rtt: 488.429197ms
clients: 17000  95per-rtt: 358.373203ms min-rtt: 78.233562ms  median-rtt: 190.05611ms max-rtt: 541.543546ms
clients: 18000  95per-rtt: 333.99741ms  min-rtt: 73.940687ms  median-rtt: 213.257192ms  max-rtt: 351.386405ms
clients: 19000  95per-rtt: 468.706693ms min-rtt: 71.66733ms median-rtt: 215.66554ms max-rtt: 477.565136ms
clients: 20000  95per-rtt: 365.298961ms min-rtt: 131.797097ms median-rtt: 219.539695ms  max-rtt: 465.667041ms
clients: 21000  95per-rtt: 418.639549ms min-rtt: 74.855777ms  median-rtt: 254.465326ms  max-rtt: 541.566808ms
clients: 22000  95per-rtt: 412.041654ms min-rtt: 111.095531ms median-rtt: 251.861027ms  max-rtt: 429.882772ms
clients: 23000  95per-rtt: 397.498533ms min-rtt: 153.165965ms median-rtt: 264.671607ms  max-rtt: 418.540737ms
clients: 24000  95per-rtt: 475.072114ms min-rtt: 88.942641ms  median-rtt: 289.662693ms  max-rtt: 482.624173ms
clients: 25000  95per-rtt: 470.695838ms min-rtt: 76.527763ms  median-rtt: 281.647172ms  max-rtt: 524.262378ms
clients: 26000  95per-rtt: 450.70621ms  min-rtt: 135.185676ms median-rtt: 298.69296ms max-rtt: 472.417507ms
```

### Memory Usage

Approximate memory usage as eyeballed in htop.

* Go       ~800MB
* C++       ~30MB
* Clojure ~1500MB
* Rails    ~150MB
* Elixir  ~1900MB
