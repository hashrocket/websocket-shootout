defmodule PhoenixSocket.PageController do
  use PhoenixSocket.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
