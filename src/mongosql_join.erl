%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc MongoSQL JOIN
%% Implements various types of JOIN using naive nested loop algorithm

-module(mongosql_join).

-export([natural/2, equi/3, cross/2]).
-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").

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
                                  fun({R_V, S_V}) -> element(R_Index, R_V) ==
                                                         element(S_Index, S_V)
                                  end
                      end, Using),
    Predicate = fun({R_V, S_V}) -> all(Preds, {R_V, S_V}) end,
    Dups = dups(S_Col, R_Col),
    Columns = concat2(R_Col, dedup(S_Col, Dups)),
    Values = [concat2(R_V, dedup(S_V, Dups)) || R_V <- values(R), S_V <- values(S),
                                         Predicate({R_V, S_V})],
    {selected, Columns, Values}.

%% SELECT * FROM employee CROSS JOIN department;
%% SELECT * FROM employee, department;
cross(R, S) ->
    Columns = concat2(columns(R), columns(S)),
    Values = [concat2(R_V, S_V) || R_V <- values(R), S_V <- values(S)],
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

dedup(List, Dups) when is_list(List) ->
    Dedup = lists:filter(fun(Tuple) ->
                                 element(2, Tuple) end, lists:zip(List, Dups)),
    lists:map(fun(Tuple) -> element(1, Tuple) end, Dedup);
dedup(List, Dups) when is_tuple(List) ->
    list_to_tuple(dedup(tuple_to_list(List), Dups)).

concat2(A, B) when is_list(A), is_list(B) ->
    lists:concat([A, B]);
concat2(A, B) when is_tuple(A), is_tuple(B) ->
    list_to_tuple(concat2(tuple_to_list(A), tuple_to_list(B))).

all(Predicates, Arg) ->
    lists:foldl(fun(Pred, Res) -> Pred(Arg) and Res end, true, Predicates).

any(Predicates, Arg) ->
    lists:foldl(fun(Pred, Res) -> Pred(Arg) or Res end, false, Predicates).

test_setup() ->
    Employee = {selected, ["LastName", "DepartmentID"], 
                [{"Rafferty", "31"},
                 {"Jones", "33"},
                 {"Steinberg", "33"},
                 {"Robinson", "34"},
                 {"Smith", "34"},
                 {"John", "NULL"}]},
    Department = {selected, ["DepartmentID", "DepartmentName"], 
                  [{"31", "Sales"},
                   {"33", "Engineering"},
                   {"34", "Clerical"}, 
                   {"35", "Marketing"}]},
    {Employee, Department}.

cross_test_() ->
    {Employee, Department} = test_setup(),
    Cross1 = {"Rafferty","31","31","Sales"},
    [?_assertEqual(length(values(cross(Employee, Department))),
                   length(values(Employee)) * length(values(Department))),
     ?_assertEqual(hd(values(cross(Employee, Department))), Cross1)].

equi_test() ->
    {Employee, Department} = test_setup(),
    Equi = {selected, ["LastName","DepartmentID","DepartmentName"],
               [{"Rafferty", "31", "Sales"},
                {"Jones", "33", "Engineering"},
                {"Steinberg", "33", "Engineering"},
                {"Robinson", "34", "Clerical"},
                {"Smith", "34", "Clerical"}]},
    ?_assertEqual(equi(Employee, Department, ["DepartmentID"]), Equi).

natural_test() ->
    {Employee, Department} = test_setup(),
    Natural = {selected, ["LastName","DepartmentID","DepartmentName"],
               [{"Rafferty", "31", "Sales"},
                {"Jones", "33", "Engineering"},
                {"Steinberg", "33", "Engineering"},
                {"Robinson", "34", "Clerical"},
                {"Smith", "34", "Clerical"}]},
    ?_assertEqual(natural(Employee, Department), Natural).
