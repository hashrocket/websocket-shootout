
require 'agoo'
require 'shooter'

Agoo::Server.init(9292, '.', thread_count: 0)

Agoo::Server.handle(:GET, "/ws", Shooter)
Agoo::Server.start()

# To run this for the websocket-shootout benchmarking tool:
# ruby shootout.rb
