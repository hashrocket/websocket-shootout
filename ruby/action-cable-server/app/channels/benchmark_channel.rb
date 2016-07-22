class BenchmarkChannel < ApplicationCable::Channel
  def subscribed
    Rails.logger.info "a client subscribed: #{id}"
    stream_from id
    stream_from "all"
  end

  def echo(data)
    ActionCable.server.broadcast id, data
  end

  def broadcast(data)
    ActionCable.server.broadcast "all", data
    data["action"] = "broadcastResult"
    ActionCable.server.broadcast id, data
  end
end
