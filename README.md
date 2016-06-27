# websocket-shootout

Environment setup (from project root)

```
export GOPATH=`pwd`/go
export PATH=$GOPATH/bin:$PATH
```

## Open file limits

If you try to benchmark with many connections you will probably run into OS level open file limits. Here is how to increase those limits.

### Ubuntu 14.04

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
