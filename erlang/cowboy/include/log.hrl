%% Copyright
-author("palkan").

-ifdef(TEST).

-define(D(X), lager:info("[DEBUG] ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(I(X), lager:info("[INFO] ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(E(X), lager:info("[ERROR] ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-else.

-define(D(X), lager:debug("~p:~p ~p",[?MODULE, ?LINE, X])).
-define(I(X), lager:info("~p:~p ~p",[?MODULE, ?LINE, X])).
-define(E(X), lager:error("~p:~p ~p",[?MODULE, ?LINE, X])).

-endif.
