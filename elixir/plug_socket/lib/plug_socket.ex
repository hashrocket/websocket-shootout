defmodule PlugSocket do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    opts = [strategy: :one_for_one, name: PlugSocket.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp children do
    [cowboy]
  end

  defp cowboy do
      Plug.Adapters.Cowboy.child_spec(:http, PlugSocket.Router, [], port: 4000, dispatch: dispatch)
  end

  defp dispatch do
    [
      {:_, [
          {"/ws", PlugSocket.SocketHandler, []},
          {:_, Plug.Adapters.Cowboy.Handler, {PlugSocket.Router, []}}
        ]}
    ]
  end

end
