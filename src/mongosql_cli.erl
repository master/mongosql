%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc MongoSQL CLI
-module(mongosql_cli).

-export([init/0]).

loop(Pool) ->
    case string:strip(io:get_line("> ")) of
	O when O == "exit"; O == "quit" ->
	    halt();
	Str ->
	    Res = mongosql_conn:fetch(Pool, Str, false),	    
	    io:format("~p~n", [Res])
    end,
    loop(Pool).

init() ->
    application:start(emongo),
    {ok, Pool} = mongosql_conn:start("127.0.0.1", 27017, "db1"),
    loop(Pool).
