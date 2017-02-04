-module(ws_shootout_server).
-behaviour(gen_server).
-behaviour(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("ws_shootout/include/log.hrl").
-include_lib("ws_shootout/include/priv.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0]).
%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #{}}.

authorize(#de_client{socket = Socket}=Client,_) ->
  gen_server:call(?SERVER, {join, Socket}),
  {ok, Client}.

handle_message(_,_) -> ok.

handle_client_message(Client, Raw) ->
  Message = json_encoder:decode(Client, Raw),
  gen_server:call(?SERVER, {handle_client_message, Message, Client, Raw});

handle_client_message(_, _) -> ok.

client_disconnected(#de_client{socket = Socket}=Client) -> 
  gen_server:call(?SERVER, {leave, Socket}),
  ok.

handle_call({handle_client_message, #{ <<"type">> := <<"echo">> }, Client, Raw}, _, State) ->
  {reply, {reply, Client, Raw}, State};

handle_call({handle_client_message, #{ <<"type">> := <<"broadcast">> } = Data, Client, Raw}, _, State) ->
  self() ! {broadcast, Client, Data, Raw},
  {reply, ok, State};

handle_call({join, Socket}, _From, State) ->
  {reply, ok, State#{ Socket => 1}};

handle_call({leave, Socket}, _From, State) ->
  {reply, ok, maps:remove(Socket, State)}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({broadcast, Client, Data, Raw}, Clients) ->
  [Socket ! {handle_message, Raw} || Socket <- maps:keys(Clients)],
  Reply = Data#{ <<"type">> => <<"broadcastResult">>},
  de_client:send(Client#de_client{encoder=json_encoder}, Reply),
  {noreply, Clients};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
