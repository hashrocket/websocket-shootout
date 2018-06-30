# Shootout with an Agoo shooter

Agoo shows it's flexibility by being a team player and working with Rack as
well shooting by itself with the `shootout.rb` file. Results for both
approaches are similar as the Rack overhead is only present when creating new
connections.

The `Shooter` class is central to both approaches and so it is in a file on
it's own. To bring the shooter to life change directories to the `ruby/agoo`
directory and type:

```
ruby -I. shootout.rb
```

Or to run with Rack type:

```
rackup -I. -r agoo -s agoo
```

Then run the websocket-bench tool from the repo root in another terminal
preferably on a different machine. Of course if on a different machine use the
correct IP address for that machine.

```
bin/websocket-bench broadcast --step-size 1000 -s 100 ws://127.0.0.1:9292/ws

```
