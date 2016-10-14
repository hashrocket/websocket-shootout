defmodule PlugSocket.Mixfile do
  use Mix.Project

  def project do
    [app: :plug_socket,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [
      applications: [
        :logger,
        :plug,
        :cowboy,
        :gproc,
      ],
      mod: {PlugSocket, []}]
  end

  defp deps do
    [
      {:cowboy, "~> 1.0.0"},
      {:plug, "~> 1.0"},
      {:poison, "~> 3.0"},
      {:gproc, "~> 0.6"},
    ]
  end
end
