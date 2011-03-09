%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc MongoSQL semantic clauses
-module(mongosql_sem).

-export([compile/1, compile/2, compile_all/2]).

compile_all([Token|Tail], Acc) -> 
    compile_all(Tail, [compile(Token)|Acc]);
compile_all([], Acc) -> 
    lists:reverse(Acc).

compile(Tokens) when is_list(Tokens) -> compile_all(Tokens, []);

compile({delete, Table, Where}) -> 
    {delete, compile(Table), compile(Where)};

compile({insert, Table, Fields, Values}) -> 
    {insert, compile(Table), lists:zip(compile(Fields), compile(Values))};

compile({select,_, {count}, 
	 {Table, Where,_OrderBy,_GroupBy,_Having,_Limit,_Offset}}) -> 
    {count, compile(Table), compile(Where)};

compile({select,_, Fields, 
	 {Table, Where, OrderBy,_GroupBy,_Having, Limit, Offset}}) -> 
    {find, compile(Table), compile(Where), lists:flatten([compile(OrderBy),
							  compile(Limit),
							  compile(Offset),
							  compile(Fields)])};

compile({update, Table, Assign, Where}) ->
    {update, compile(Table), compile(Where), [{<<"$set">>, compile(Assign)}]};

compile({selection, '*'}) -> [];
compile({selection, Arg}) -> [{fields, Arg}];
compile({orderby, Arg}) -> [{orderby, Arg}];
compile({limit, Arg}) -> [{limit, Arg}];
compile({offset, Arg}) -> [{offset, Arg}];
compile({assign, Arg1, Arg2}) -> {compile(Arg1), compile(Arg2)};

compile({'and', Arg1, Arg2}) -> compile(Arg1) ++ compile(Arg2);
compile({'or', Arg1, Arg2}) -> [{<<"$or">>, [compile(Arg1), compile(Arg2)]}];
compile({'not', Arg}) -> [{<<"$not">>, compile(Arg)}];

compile({'=', Arg1, Arg2}) -> [{compile(Arg1), compile(Arg2)}];
compile({'>', Arg1, Arg2}) -> [{compile(Arg1), [{gt, compile(Arg2)}]}];
compile({'<', Arg1, Arg2}) -> [{compile(Arg1), [{lt, compile(Arg2)}]}];
compile({'>=', Arg1, Arg2}) -> [{compile(Arg1), [{gte, compile(Arg2)}]}];
compile({'<=', Arg1, Arg2}) -> [{compile(Arg1), [{lte, compile(Arg2)}]}];
compile({'<>', Arg1, Arg2}) -> [{compile(Arg1), [{ne, compile(Arg2)}]}];

compile({between, Arg1, Arg2, Arg3}) -> 
    [{compile(Arg1), [{gte, compile(Arg2)}, {lte, compile(Arg3)}]}];
compile({notbetween, Arg1, Arg2, Arg3}) -> 
    [{compile(Arg1), [{gte, compile(Arg3)}, {lte, compile(Arg2)}]}];

compile({like, Arg1, Arg2}) -> [{compile(Arg1), [{regexp, compile(Arg2), ""}]}];
%% @todo '$not' with '$regex' is not supported by MongoDB
%% compile({notlike,_Arg1,_Arg2}) -> false; 

compile({null, Arg}) -> [{compile(Arg), [{exists, false}]}];
compile({notnull, Arg}) -> [{compile(Arg), [{exists, true}]}];

compile({in, Arg1, Arg2}) -> [{compile(Arg1), [{in, compile(Arg2)}]}];
compile({notin, Arg1, Arg2}) -> [{compile(Arg1), [{nin, compile(Arg2)}]}];

compile({'+', Arg1, Arg2}) -> compile(Arg1) + compile(Arg2);
compile({'-', Arg1, Arg2}) -> compile(Arg1) - compile(Arg2);
compile({'*', Arg1, Arg2}) -> compile(Arg1) * compile(Arg2);
compile({'/', Arg1, Arg2}) -> compile(Arg1) / compile(Arg2);

compile(nil) -> [];

compile(Token) when is_atom(Token) -> Token;
compile(Token) when is_integer(Token) -> Token;
compile(Token) when is_bitstring(Token) -> Token;
compile(Token) when is_list(Token) -> Token;

compile(Token) -> {unknown_token, Token}.

%% JavaScript semantic

compile({delete, Table, Where}, js) -> 
    flatten(["db.", compile(Table, js), 
	     ".delete(", compile({where, Where}, js), ")"]);

compile({insert, Table, Fields, Values}, js) -> 
    flatten(["db.", compile(Table, js), 
	     ".save(", list_to_obj(lists:zip(compile(Fields, js), 
					     compile(Values, js))), ")"]);

compile({select,_, {count}, 
	 {Table, Where,_OrderBy,_GroupBy,_Having,_Limit,_Offset}}, js) -> 
    flatten(["db.", compile(Table, js), 
	     ".count(", compile({where, Where}, js), ")"]);

compile({select,_, Fields, 
	 {Table, Where, OrderBy,_GroupBy,_Having, Limit, Offset}}, js) -> 
    flatten(["db.", compile(Table, js),
	     ".find(", compile({where, Where}, js), ",",
	     list_to_obj(compile(Fields, js)), ")",
	     compile(OrderBy), compile(Limit), compile(Offset)]);

compile({update, Table, Assign, Where}, js) ->
    flatten(["db.", compile(Table, js), 
	     ".update(", compile({where, Where}, js),
	     ", {'$set': ", compile(Assign, js), "}, false, true)"]);

compile({where, Arg}, js) ->
    flatten(["{ $where: \"",  compile(Arg), "\" } )"]);

compile({orderby, Arg}, js) ->
    flatten([".sort(",  list_to_obj(compile(Arg, js)), ")"]);
compile({limit, Arg}, js) -> 
    flatten([".limit(",  compile(Arg, js), ")"]);
compile({offset, Arg}, js) -> 
    flatten([".skip(",  compile(Arg, js), ")"]);

%%compile({assign, Arg1, Arg2}, js) -> {compile(Arg1), compile(Arg2)};

compile({asc, Arg}, js) ->
    {compile(Arg, js), 1};
compile({desc, Arg}, js) ->
    {compile(Arg, js), -1};

compile({'and', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") and (", compile(Arg2, js), ")"]);
compile({'or', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") or (", compile(Arg2, js), ")"]);
compile({'not', Arg1}, js) -> 
    flatten(["not (", compile(Arg1, js), ")"]);

compile({'=', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") == (", compile(Arg2, js), ")"]);
compile({'>', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") > (", compile(Arg2, js), ")"]);
compile({'<', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") < (", compile(Arg2, js), ")"]);
compile({'>=', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") >= (", compile(Arg2, js), ")"]);
compile({'<=', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") <= (", compile(Arg2, js), ")"]);
compile({'<>', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") != (", compile(Arg2, js), ")"]);

compile({between, Arg1, Arg2, Arg3}, js) -> 
    compile({'and',
	     compile({'>=', Arg1, Arg2}, js), compile({'<=', Arg1, Arg3}, js)}, js);
compile({notbetween, Arg1, Arg2, Arg3}, js) -> 
    compile({'not', compile({between, Arg1, Arg2, Arg3}, js)}, js);

compile({like, Arg1, Arg2}, js) -> 
    compile({notnull, 
	     flatten([compile(Arg1, js), ".match(", compile(Arg2, js), ")"])}, js);
compile({notlike, Arg1, Arg2}, js) ->
    compile({'not', compile({like, Arg1, Arg2}, js)}, js);

compile({null, Arg}, js) -> 
    compile({'==', compile(Arg, js), "null"}, js);
compile({notnull, Arg}, js) -> 
    compile({'!=', compile(Arg, js), "null"}, js);

compile({in, Arg1, Arg2}, js) -> 
    flatten([compile(Arg1, js), " in ", compile(Arg2, js)]);
compile({notin, Arg1, Arg2}, js) -> 
    compile({'not', compile({in, Arg1, Arg2}, js)}, js);

compile({'+', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") + (", compile(Arg2, js), ")"]);
compile({'-', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") - (", compile(Arg2, js), ")"]);
compile({'*', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") * (", compile(Arg2, js), ")"]);
compile({'/', Arg1, Arg2}, js) -> 
    flatten(["(", compile(Arg1, js), ") / (", compile(Arg2, js), ")"]);

compile(nil, js) -> "";

compile(Token, js) when is_atom(Token) -> 
    "this." + atom_to_list(Token);
compile(Token, js) when is_integer(Token) -> 
    integer_to_list(Token);
compile(Token, js) when is_bitstring(Token) ->
    "\"" + bitstring_to_list(Token) + "\"";
compile(Token, js) when is_list(Token) -> 
    Token;

compile(Token, js) -> 
    {unknown_token, Token}.

flatten(List) -> string:join(List, "").

list_to_obj(List) when is_list(List) ->
    Items = string:join(lists:map(fun(R) -> 
					  flatten([element(1, R), ": ", element(2, R)])
				  end, List), ", "),
    flatten(["{", Items, "}"]).
