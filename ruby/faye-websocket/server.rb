require 'faye/websocket'
require 'json'
require 'set'

class Server
  def initialize
    @conns = Set.new
  end

  def call(env)
    if Faye::WebSocket.websocket?(env)
      ws = Faye::WebSocket.new(env)

      ws.on :open do |event|
        @conns.add(ws)
      end

      ws.on :message do |event|
        cmd, payload = JSON(event.data).values_at('type', 'payload')
        if cmd == 'echo'
          ws.send({type: 'echo', payload: payload}.to_json)
        else
          msg = {type: 'broadcast', payload: payload}.to_json
          @conns.each { |c| c.send(msg) }
          ws.send({type: "broadcastResult", payload: payload}.to_json)
        end
      end

      ws.on :close do |event|
        @conns.delete(ws)
      end

      ws.rack_response
    else
      [200, {'Content-Type' => 'text/plain'}, ['Please connect a websocket client']]
    end
  end
end
