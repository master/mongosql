%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc MongoSQL connection
-module(mongosql_conn).

-export([start/3, fetch/3, stop/1]).

-define(POOL_SIZE, 3).

squery(Pool, {delete, Coll, Selector}, false) -> 
    emongo:delete(Pool, Coll, Selector),
    {updated, 1};

squery(Pool, {delete, Coll, Selector}, true) -> 
    parse_result(emongo:delete_sync(Pool, Coll, Selector));

squery(Pool, {insert, Coll, Doc}, false) ->
    emongo:insert(Pool, Coll, Doc),
    {updated, 1};

squery(Pool, {insert, Coll, Doc}, true) ->
    case parse_result(emongo:insert_sync(Pool, Coll, Doc)) of
	{updated, 0} -> {updated, 1};
	R -> R
    end;

squery(Pool, {count, Coll, Selector},_) -> 
    case emongo:count(Pool, Coll, Selector) of
	Cnt when is_integer(Cnt) ->
            {selected,["count(*)"], [{integer_to_list(Cnt)}]};
        _Else -> {selected,["count(*)"], [{"0"}]}
    end;

squery(Pool, {find, Coll, Selector, Options},_) -> 
    Res = emongo:find_all(Pool, Coll, Selector, Options),
    Norm = filter_null(filter_id(Res)),
    Fields = lists:keyfind(fields, 1, Options),
    {selected, selected_rows(Norm, Fields), row_values(Norm, Fields)};

squery(Pool, {update, Coll, Selector, Doc}, false) -> 
    emongo:update(Pool, Coll, Selector, Doc, false, true),
    {updated, 1};

squery(Pool, {update, Coll, Selector, Doc}, true) -> 
    parse_result(emongo:update_sync(Pool, Coll, Selector, Doc, false, true));

squery(_,[],_) -> 
    {updated, 0};

squery(_,_,_) -> 
    {error, "Unknown SQL token"}.

parse_result(Result) ->
    Res = lists:flatten(Result),
    case lists:keyfind(<<"err">>, 1, Res) of
	{<<"err">>, null} ->
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

row_values(Cols, false) ->
    lists:map(
      fun(R) -> list_to_tuple(
		  lists:map(
		    fun(C) -> bin_to_str(element(2, C)) end, R))
      end, Cols);

row_values(Cols, {fields, Fields}) ->
    Sorted = lists:map(
	       fun(R) -> lists:map(
			   fun(K) -> lists:keyfind(K, 1, R) end,
			   Fields) 
	       end, Cols),
    row_values(Sorted, false).

selected_rows(_, {fields, Fields}) ->
    lists:map(fun(C) -> bin_to_str(C) end, Fields);

selected_rows([],_) ->
    [];

selected_rows(Cols, false) ->
    lists:map(
      fun(C) -> bin_to_str(element(1, C)) end, 
      hd(Cols)).

bin_to_str(S) when is_bitstring(S) ->
    bitstring_to_list(S);

bin_to_str(S) -> S.

%% @doc Connect to database
start(Host, Port, Database) ->
    start([{Host, Port}], Database).

start(Urls, Database) when is_list(Urls) ->
    emongo:add_pool(make_ref(), Urls, Database, ?POOL_SIZE).

%% @doc Execute SQL query
fetch(PoolId, Query, Sync) ->
    case sql92_scan:string(lists:flatten(Query)) of
	{ok, Toks,_} ->
	    case sql92_parser:parse(Toks) of
		{ok, Ast} ->
		    Qs = mongosql_sem:compile(Ast),
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
