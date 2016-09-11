-module(ws_shootout_app).
-author(palkan).
-include_lib("ws_shootout/include/log.hrl").
-include_lib("ws_shootout/include/priv.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ?I("Starting application ws_shootout on /ws/cable"),
  deliverly:register_handler(cable, ws_shootout_server),
  ws_shootout_sup:start_link().

stop(_State) ->
  ok.
