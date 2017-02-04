%% Copyright
-module(ws_shootout).
-author(palkan).
-include_lib("ws_shootout/include/log.hrl").
-include_lib("ws_shootout/include/priv.hrl").
-define(APPS, [lager, deliverly, pg2]).

%% ------------------------------------------------------------------
%% Common Application Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0, upgrade/0, ping/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-define(SERVER, ws_shootout_server).

start() ->
  ulitos_app:ensure_started(?APPS),
  application:start(ws_shootout).

stop() ->
  application:stop(ws_shootout).

upgrade() ->
 ulitos_app:reload(ws_shootout),
 ok.
 
ping() ->
  pong.
