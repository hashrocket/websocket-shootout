
# We'll use the Iodine server and leverage it's features.
require 'iodine'
require 'json'

module App
  # HTTP requests use class methods
  def self.call(env)
    if env['upgrade.websocket?'] # an upgrade request
      env['upgrade.websocket'] = Client.new(env) # create a Websocket handler
    end
    [200, {'Content-Type' => 'text/plain', 'Content-Length' => "26"}, ["Use websockets to connect."]]
  end
end

class Client
  def initialize(env)
    @env = env
  end

  def on_message data
    cmd, payload = JSON(data).values_at('type', 'payload')
    if cmd == 'echo'
      write({type: 'echo', payload: payload}.to_json)
    else
      msg = {type: 'broadcast', payload: payload}.to_json
      # we use the class method since the instance `#each` method excludes `self`
      # NOTE: `each` is concurrent and might return before the actual broadcast had completed.
      Client.each { |client| client.write(msg) }
      write({type: "broadcastResult", payload: payload}.to_json)
    end
  end
end

# # enforce concurrency when not using command line directives
# Iodine.threads = 8

# Set the Rack application
run App

# # if using the `irb` terminal, use:
# Iodine::Rack.app = App
# Iodine.start

