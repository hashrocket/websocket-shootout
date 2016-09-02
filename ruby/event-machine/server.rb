require 'em-websocket'
require 'json'
require 'optparse'

address = "0.0.0.0"
port = 8080

OptionParser.new do |opts|
  opts.banner = "Usage: bundle exec server.rb [options]"

  opts.on("-a", "--address", "Address") do |a|
    address = a
  end

  opts.on("-p", "--port PORT", Integer, "Port") do |p|
    port = Integer(p)
  end
end.parse!

EM.run {
  @channel = EM::Channel.new

  EM::WebSocket.run(:host => address, :port => port) do |ws|
    ws.onopen {
      sid = @channel.subscribe {|msg| ws.send msg }
      @channel.push "#{sid} connected"
    }

    ws.onmessage { |msg|
      cmd, payload = JSON(msg).values_at('type', 'payload')
      if cmd == 'echo'
        ws.send payload.to_json
      else
        @channel.push payload.to_json
      end
    }
  end
}
