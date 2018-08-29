require 'oj'

# Keep it simple and just use the class since the use is stateless.
class Shooter
  def self.call(env)
    unless env['rack.upgrade?'].nil?
      env['rack.upgrade'] = self
      [ 200, { }, [ ] ]
    else
      [ 404, { }, [ ] ]
    end
  end

  def self.on_open(client)
    client.subscribe('shootout')
  end

  def self.on_close(client)
    client.unsubscribe('shootout')
  end

  def self.on_message(client, data)
    cmd, payload = Oj.load(data).values_at('type', 'payload')
    if cmd == 'echo'
      client.write(data)
    else
      Agoo.publish('shootout', data)
      client.write(Oj.dump({type: "broadcastResult", payload: payload}, mode: :strict))
    end
  end
end
