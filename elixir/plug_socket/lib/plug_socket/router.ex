defmodule PlugSocket.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  match(_) do
    conn
    |> send_resp(200, "hello")
  end

end
