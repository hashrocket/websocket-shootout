defmodule PlugSocket.SocketHandler do
  @behaviour :cowboy_websocket_handler

  def init(_, _req, _opts) do
    {:upgrade, :protocol, :cowboy_websocket}
  end

  def websocket_init(_type, req, _opts) do
    :gproc.reg({:p, :l, :plug_socket})
    {:ok, req, %{}, 60_000_000}
  end

  def websocket_info({:send, message}, req, state) do
    {:reply, {:text, message}, req, state}
  end

  def websocket_handle({:text, message}, req, state) do
    message
    |> Poison.decode!()
    |> respond()
    |> Tuple.append(req)
    |> Tuple.append(state)
  end

  defp respond(%{"type"=> "echo", "payload" => payload}) do
    {:reply, {:text, payload}}
  end

  defp respond(%{"type"=> "broadcast", "payload" => payload}) do
    broadcast = Poison.encode!(%{type: "broadcast", payload: payload})
    :gproc.send({:p, :l, :plug_socket}, {:send, broadcast})
    broadcast_result = Poison.encode!(%{type: "broadcastResult", payload: payload})
    {:reply, {:text, broadcast_result}}
  end

  # No matter why we terminate, remove all of this pids subscriptions
  def websocket_terminate(_reason, _req, _state) do
    :ok
  end
end
