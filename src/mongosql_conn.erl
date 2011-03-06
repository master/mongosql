%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc MongoSQL connection
-module(mongosql_conn).

-export([start/3, start/5, fetch/3, stop/1]).

squery(Pool, {delete, Coll, Selector}, false) -> 
    emongo:delete(Pool, Coll, Selector),
    {updated, 1};

squery(Pool, {delete, Coll, Selector}, true) -> 
    parse_result(emongo:delete_sync(Pool, Coll, Selector));

squery(Pool, {insert, Coll, Doc}, false) ->
    emongo:insert(Pool, Coll, Doc),
    {updated, 1};

squery(Pool, {insert, Coll, Doc}, true) ->
    parse_result(emongo:insert_sync(Pool, Coll, Doc));

squery(Pool, {count, Coll, Selector},_) -> 
    Cnt = emongo:count(Pool, Coll, Selector),
    {selected,["count(*)"], [{Cnt}]};

squery(Pool, {find, Coll, Selector, Options},_) -> 
    Res = emongo:find_all(Pool, Coll, Selector, Options),
    Norm = filter_null(filter_id(Res)),
    {selected, selected_rows(Norm), row_values(Norm)};

squery(Pool, {update, Coll, Selector, Doc}, false) -> 
    emongo:update(Pool, Coll, Selector, Doc, false),
    {updated, 1};

squery(Pool, {update, Coll, Selector, Doc}, true) -> 
    parse_result(emongo:update(Pool, Coll, Selector, Doc, false));

squery(_,_,_) -> 
    {error, "Unknown SQL token"}.

parse_result(Result) ->
    Res = lists:flatten(Result),
    case lists:keyfind(<<"err">>, 1, Res) of
	{<<"err">>, undefined} ->
	    case lists:keyfind(<<"n">>, 1, Res) of
		{<<"n">>, N} ->
		    {updated, N};
		_ ->
		    {updated, 0}
	    end;
	{<<"err">>, Error} ->
	    {error, Error};
	_ ->
	    {error, "Error parsing result"}
    end.

filter_id(Cols) ->
    lists:map(
      fun(R) -> lists:filter(
		  fun(C) -> element(1, C) /= <<"_id">> end, R) 
      end, Cols).

filter_null(Cols) ->
    lists:filter(
      fun(L) -> L /= [] end, Cols).

row_values(Cols) ->
    lists:map(
      fun(R) -> lists:map(
		  fun(C) -> bin_to_str(element(2, C)) end, R)
      end, Cols).

selected_rows([]) ->
    [];

selected_rows(Cols) ->
    lists:map(
      fun(C) -> bin_to_str(element(1, C)) end, 
      hd(Cols)).

bin_to_str(S) when is_bitstring(S) ->
    bitstring_to_list(S);

bin_to_str(S) -> S.

%% @doc Connect to database
start(Host, Port, Database) ->
    start(Host, Port, "", "", Database).

start(Host, Port,_User,_Password, Database) ->
    PoolId = list_to_atom(Host ++ integer_to_list(Port) ++ Database),
    emongo:add_pool(PoolId, Host, Port, Database, 1),
    {ok, PoolId}.

%% @doc Execute SQL query
fetch(PoolId, Query, Sync) ->
    case sql92_scan:string(Query) of
	{ok, Toks,_} ->
	    case sql92_parser:parse(Toks) of
		{ok, Ast} ->
		    Qs = mongosql_syn:compile(Ast),
		    lists:foldl(fun(Q,_) -> squery(PoolId, Q, Sync) end,
				{error, "Error executing query"}, Qs);
		_ ->
		    {error, "Error in query syntax"}
	    end;
	_ -> 
	    {error, "Error in query grammar"}
    end.

%% @doc Disconnect from database
stop(PoolId) ->
    emongo:del_pool(PoolId).    
		
    
