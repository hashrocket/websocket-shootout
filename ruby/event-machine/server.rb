require 'em-websocket'
require 'json'

EM.run {
  @channel = EM::Channel.new

  EM::WebSocket.run(:host => "0.0.0.0", :port => 8080) do |ws|
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
