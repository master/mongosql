%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc A syntax of a subset of SQL-92

Nonterminals
assign_commalist assign atom atom_commalist between_pred column 
column_commalist column_ref column_ref_commalist comparsion_pred 
delete_stmt from_clause in_pred insert_atom opt_column_commalist
insert_atom_commalist insert_stmt like_pred literal opt_order_by_clause
manipulative_stmt opt_all_distinct opt_group_by_clause sort_spec_commalist
sort_spec sort_key ordering_spec opt_having_clause opt_where_clause
predicate range_variable scalar_exp scalar_exp_commalist 
search_cond selection select_stmt sql sql_list table table_exp
table_ref table_ref_commalist test_for_null_pred update_stmt 
values_or_select_stmt where_clause opt_limit_clause opt_offset_clause
transaction_stmt.

Terminals '-' '+' '*' '/' '(' ')' ',' ';' '='
delete insert select update from where into values all null not in count
like between or and group by distinct having is set offset limit
begin rollback commit order asc desc comp string name integer.

Rootsymbol sql_list.

%% Top level rules

sql_list -> sql : ['$1'].
sql_list -> sql_list ';' sql : flatten(['$1', '$3']).
sql_list -> sql_list ';' : '$1'.

sql -> manipulative_stmt : '$1'.

manipulative_stmt -> transaction_stmt : '$1'.
manipulative_stmt -> delete_stmt : '$1'.
manipulative_stmt -> insert_stmt : '$1'.
manipulative_stmt -> select_stmt : '$1'.
manipulative_stmt -> update_stmt : '$1'.

%% Transactions

transaction_stmt -> begin : nil.
transaction_stmt -> rollback : nil.
transaction_stmt -> commit : nil.

%% DELETE

delete_stmt -> delete from table opt_where_clause : {delete, '$3', '$4'}.

%% INSERT statement

insert_stmt -> insert into table opt_column_commalist 
	    values_or_select_stmt : {insert, '$3', '$4', '$5'}.

opt_column_commalist -> '$empty' : nil.
opt_column_commalist -> '(' column_commalist ')' : '$2'.
opt_column_commalist -> column_commalist : '$1'.

values_or_select_stmt -> values '(' insert_atom_commalist ')'
		       : '$3'.
values_or_select_stmt -> select_stmt : '$1'.

insert_atom_commalist -> insert_atom : ['$1'].
insert_atom_commalist -> insert_atom_commalist ',' insert_atom
		       : flatten(['$1', '$3']).

insert_atom -> '$empty' : nil.
insert_atom -> atom : '$1'.

atom -> literal : '$1'.

%% SELECT statement

select_stmt -> select opt_all_distinct selection table_exp
	     : {select, '$2', '$3', '$4'}.

opt_all_distinct -> '$empty' : nil.
opt_all_distinct -> all : all.
opt_all_distinct -> distinct : distinct.

selection -> scalar_exp_commalist : {selection, '$1'}.
selection -> '*' : {selection, '*'}.
selection -> count '(' '*' ')' : {count}.

%% UPDATE

update_stmt -> update table set assign_commalist 
	    opt_where_clause : {update, '$2', '$4', '$5'}.

assign_commalist -> assign : ['$1'].
assign_commalist -> assign_commalist ',' assign : flatten(['$1', '$3']).

assign -> column '=' scalar_exp : {assign, '$1', '$3'}.

%% Base tables

table -> name : value_of('$1').

column_commalist -> column : ['$1'].
column_commalist -> column_commalist ',' column : flatten(['$1', '$3']).

column -> name : value_of('$1').

literal -> integer : value_of('$1').
literal -> string : value_of('$1').

column_ref -> name : value_of('$1').

%% Table expressions

table_exp -> from_clause opt_where_clause opt_order_by_clause
	  opt_group_by_clause opt_having_clause opt_limit_clause
	  opt_offset_clause : {'$1', '$2', '$3', '$4', '$5', '$6', '$7'}.

from_clause -> from table_ref_commalist : '$2'.

table_ref_commalist -> table_ref : '$1'.
table_ref_commalist -> table_ref_commalist ',' table_ref
		     : flatten(['$1', '$3']).

table_ref -> table : '$1'.
table_ref -> table range_variable : {'$1', '$2'}.

range_variable -> name : '$1'.

opt_where_clause -> '$empty' : nil.
opt_where_clause -> where_clause : '$1'.

where_clause -> where search_cond : '$2'.

opt_order_by_clause -> '$empty' : nil.
opt_order_by_clause -> order by sort_spec_commalist : {orderby, '$3'}.

sort_spec_commalist -> sort_spec : ['$1'].
sort_spec_commalist -> sort_spec_commalist ',' sort_spec
		     : flatten(['$1', '$3']).

sort_spec -> sort_key : {'$1', 'asc'}.
sort_spec -> sort_key ordering_spec : {'$1', '$2'}.

sort_key -> column : '$1'.

ordering_spec -> asc : asc.
ordering_spec -> desc : desc.

opt_group_by_clause -> '$empty' : nil.
opt_group_by_clause -> group by column_ref_commalist : {groupby, '$3'}.

column_ref_commalist -> column_ref : '$1'.
column_ref_commalist -> column_ref_commalist ',' column_ref
		      : flatten(['$1', '$3']).

opt_having_clause -> '$empty' : nil.
opt_having_clause -> having search_cond : {having, '$2'}.

opt_limit_clause -> '$empty' : nil.
opt_limit_clause -> limit literal : {limit, '$2'}.

opt_offset_clause -> '$empty' : nil.
opt_offset_clause -> offset literal : {offset, '$2'}.

%% Search conditions

search_cond -> search_cond or search_cond : {'or', '$1', '$3'}.
search_cond -> search_cond and search_cond : {'and', '$1', '$3'}.
search_cond -> not search_cond : {'not', '$2'}.
search_cond -> '(' search_cond ')' : '$2'.
search_cond -> predicate : '$1'.

predicate -> comparsion_pred : '$1'.
predicate -> between_pred : '$1'.
predicate -> like_pred : '$1'.
predicate -> test_for_null_pred : '$1'.
predicate -> in_pred : '$1'.

comparsion_pred -> scalar_exp comp scalar_exp : {value_of('$2'), '$1', '$3'}.
comparsion_pred -> scalar_exp comp '(' select_stmt ')'
		 : {value_of('$2'), '$1', '$4'}.
comparsion_pred -> scalar_exp '=' scalar_exp : {'=', '$1', '$3'}.
comparsion_pred -> scalar_exp '=' '(' select_stmt ')' : {'=', '$1', '$4'}.

between_pred -> scalar_exp not between scalar_exp and scalar_exp
	      : {notbetween, '$1', '$4', '$6'}.
between_pred -> scalar_exp between scalar_exp and scalar_exp
	      : {between, '$1', '$3', '$5'}.

like_pred -> scalar_exp not like atom : {notlike, '$1', '$4'}.
like_pred -> scalar_exp like atom : {like, '$1', '$3'}.

test_for_null_pred -> column_ref is not null : {notnull, '$1'}.
test_for_null_pred -> column_ref is null : {null, '$1'}.

in_pred -> scalar_exp not in '(' select_stmt ')' : {notin, '$1', '$5'}.
in_pred -> scalar_exp in '(' select_stmt ')' : {in, '$1', '$4'}.
in_pred -> scalar_exp not in '(' atom_commalist ')' : {notin, '$1', '$5'}.
in_pred -> scalar_exp in '(' atom_commalist ')' : {in, '$1', '$4'}.

atom_commalist -> atom : ['$1'].
atom_commalist -> atom_commalist ',' atom : flatten(['$1', '$3']).

%% Scalar expressions

scalar_exp -> scalar_exp '+' scalar_exp : {'+', '$1', '$3'}.
scalar_exp -> scalar_exp '-' scalar_exp : {'-', '$1', '$3'}.
scalar_exp -> scalar_exp '*' scalar_exp : {'*', '$1', '$3'}.
scalar_exp -> scalar_exp '/' scalar_exp : {'/', '$1', '$3'}.
scalar_exp -> atom : '$1'.
scalar_exp -> column_ref : '$1'.
scalar_exp -> '(' scalar_exp ')' : '$2'.

scalar_exp_commalist -> scalar_exp : ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_exp
		      : flatten(['$1', '$3']).

Erlang code.

value_of(Token) -> element(3, Token).
flatten(List) -> lists:flatten(List).
