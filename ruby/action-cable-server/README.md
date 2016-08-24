# Rails Action Cable Server

## Dependencies

* Ruby 2.3+
* Redis

```
bundle install
```

To run the server:

```
SECRET_KEY_BASE=REPLACEME rails s -p 3334 -e production
```

The raw message to send to a connection to join a channel is as follows:

```
{"command":"subscribe","identifier":"{\"channel\":\"BenchmarkChannel\"}"}
```
