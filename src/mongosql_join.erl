%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc MongoSQL JOIN
%% Implements various types of JOIN using naive nested loop algorithm

-module(mongosql_join).

-export([natural/2, equi/3, cross/2]).

%% SELECT * FROM employee NATURAL JOIN department;
natural(R, S) ->
    R_Set = ordsets:from_list(columns(R)),
    S_Set = ordsets:from_list(columns(S)),
    equi(R, S, ordsets:intersection(R_Set, S_Set)).

%% SELECT * FROM employee INNER JOIN department USING (DepartmentID);
equi(R, S, Using) ->
    R_Col = columns(R),
    S_Col = columns(S),
    Preds = lists:map(fun(Key) -> R_Index = index_of(Key, R_Col),
                                  S_Index = index_of(Key, S_Col),
                                  fun({R_V, S_V}) ->
                                          lists:nth(R_Index, R_V) ==
                                              lists:nth(S_Index, S_V) 
                                  end
                      end, Using),
    Predicate = fun({R_V, S_V}) -> all(Preds, {R_V, S_V}) end,
    Dups = dups(S_Col, R_Col),
    Columns = R_Col ++ dedup(S_Col, Dups),
    Values = [R_V ++ dedup(S_V, Dups) || R_V <- values(R), S_V <- values(S),
                                         Predicate({R_V, S_V})],
    {selected, Columns, Values}.

%% SELECT * FROM employee CROSS JOIN department;
%% SELECT * FROM employee, department;
cross(R, S) ->
    Columns = columns(R) ++ columns(S),
    Values = [R_V ++ S_V || R_V <- values(R), S_V <- values(S)],
    {selected, Columns, Values}.

outer() ->
    ok.

%% SELECT * FROM employee LEFT OUTER JOIN department  
%%            ON employee.DepartmentID = department.DepartmentID;
left() ->
    ok.

%% SELECT * FROM employee RIGHT OUTER JOIN department 
%%            ON employee.DepartmentID = department.DepartmentID;
right() ->
    ok.

%% SELECT * FROM employee, department 
%%         WHERE employee.DepartmentID = department.DepartmentID;
inner() ->
    ok.

columns({selected, Columns, _Values}) -> Columns.

values({selected, _Columns, Values}) -> Values.

index_of(Value, List) ->
    Map = lists:zip(List, lists:seq(1, length(List))),
    case dict:find(Value, dict:from_list(Map)) of
        {ok, Index} -> Index;
        error -> notfound
    end.

dups(In, From) ->
    Set = sets:from_list(From),
    lists:map(fun(Element) -> not sets:is_element(Element, Set) end, In).

dedup(List, Dups) ->
    Dedup = lists:filter(fun(Tuple) ->
                                 element(2, Tuple) end, lists:zip(List, Dups)),
    lists:map(fun(Tuple) -> element(1, Tuple) end, Dedup).

all(Predicates, Arg) ->
    lists:foldl(fun(Pred, Res) -> Pred(Arg) and Res end, true, Predicates).

any(Predicates, Arg) ->
    lists:foldl(fun(Pred, Res) -> Pred(Arg) or Res end, false, Predicates).

test_() ->
    Employee = {selected, ["LastName", "DepartmentID"], 
                [["Rafferty", "31"],
                 ["Jones", "33"],
                 ["Steinberg", "33"],
                 ["Robinson", "34"],
                 ["Smith", "34"],
                 ["John", "NULL"]]},
    Department = {selected, ["DepartmentID", "DepartmentName"], 
                  [["31", "Sales"], 
                   ["33", "Engineering"],
                   ["34", "Clerical"], 
                   ["35", "Marketing"]]}.
