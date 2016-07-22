defmodule PhoenixSocket.Endpoint do
  use Phoenix.Endpoint, otp_app: :phoenix_socket

  socket "/socket", PhoenixSocket.UserSocket

  plug Plug.Static,
    at: "/", from: :phoenix_socket, gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Logger

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  plug Plug.Session,
    store: :cookie,
    key: "_phoenix_socket_key",
    signing_salt: "/zyr6toI"

  plug PhoenixSocket.Router
end
