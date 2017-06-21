# # Plezi Rack Application file.
# #
# # Written for Plezi v.0.15 with Iodine v.0.4
# #
# # NOTE: Plezi requires `iodine` for Websocket support.
# #       No Iodine, no websockets.

# # Run using `rackup` or using:
# iodine -t <number of threads> -w <number of processes> -p <port>
# # i.e.:
# iodine -t 8 -p 3334

# local process cluster support is built into iodine's pub/sub, but cross machine pub/sub requires Redis.
require 'plezi'

class ShootoutApp

  def index
    "This application should be used with the websocket-shootout benchmark utility."
  end
  
  def on_open
    subscribe channel: "shootout"
  end

  def on_message data

    if data[0] == 'b' # binary
      publish(channel: "shootout", message: data)
      data[0] = 'r'
      write data
      return
    end

    cmd, payload = JSON(data).values_at('type', 'payload')
    if cmd == 'echo'
      write({type: 'echo', payload: payload}.to_json)
    else
      # data = {type: 'broadcast', payload: payload}.to_json
      # broadcast :push2client, data
      publish(channel: "shootout", message: ({type: 'broadcast', payload: payload}.to_json))
      write({type: "broadcastResult", payload: payload}.to_json)
    end

  rescue
    puts "Incoming message format error!"
  end

end

Plezi.route '*', ShootoutApp

run Plezi.app
