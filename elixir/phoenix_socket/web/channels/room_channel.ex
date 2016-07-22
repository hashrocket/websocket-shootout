defmodule PhoenixSocket.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def handle_in("echo", message, socket) do
    resp = %{body: message["body"], type: "echo"}
    {:reply, {:ok, resp}, socket}
  end

  def handle_in("broadcast", message, socket) do
    bcast = %{body: message["body"], type: "broadcast"}
    broadcast! socket, "broadcast", bcast
    resp = %{body: message["body"], type: "broadcastResult"}
    {:reply, {:ok, resp}, socket}
  end
end
