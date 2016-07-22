# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :phoenix_socket,
  ecto_repos: [PhoenixSocket.Repo]

# Configures the endpoint
config :phoenix_socket, PhoenixSocket.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "OhWZp4/UFxBaSjwk22r4WK7F20sfxxwpHrj3XqKB4UkCjbsMGWJnzriZXcscaXpQ",
  render_errors: [view: PhoenixSocket.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PhoenixSocket.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
