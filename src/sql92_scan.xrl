%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc A grammar of a subset of SQL-92

Definitions.

D   = [0-9]
L   = [A-Za-z_][A-Za-z0-9_.]*
S1  = [\x20-\x26\x28-\x7E]*
S2  = [\x20-\x21\x23-\x7E]*
WS  = ([\000-\s]|%.*)
C   = (<|<=|>=|>|<>)
P   = [-+*/(),;=]

Rules.

all     : {token, {'all', TokenLine}}.
and     : {token, {'and', TokenLine}}.
asc     : {token, {'asc', TokenLine}}.
begin   : {token, {'begin', TokenLine}}.
between : {token, {'between', TokenLine}}.
by      : {token, {'by', TokenLine}}.
commit  : {token, {'commit', TokenLine}}.
count   : {token, {'count', TokenLine}}.
delete  : {token, {'delete', TokenLine}}.
desc    : {token, {'desc', TokenLine}}.
distinct : {token, {'distinct', TokenLine}}.
from    : {token, {'from', TokenLine}}.
group   : {token, {'group', TokenLine}}.
having  : {token, {'having', TokenLine}}.
insert  : {token, {'insert', TokenLine}}.
in      : {token, {'in', TokenLine}}.
into    : {token, {'into', TokenLine}}.
is      : {token, {'is', TokenLine}}.
like    : {token, {'like', TokenLine}}.
limit   : {token, {'limit', TokenLine}}.
not     : {token, {'not', TokenLine}}.
null    : {token, {'null', TokenLine}}.
offset  : {token, {'offset', TokenLine}}.
or      : {token, {'or', TokenLine}}.
order   : {token, {'order', TokenLine}}.
rollback : {token, {'rollback', TokenLine}}.
select  : {token, {'select', TokenLine}}.
set     : {token, {'set', TokenLine}}.
update  : {token, {'update', TokenLine}}.
values  : {token, {'values', TokenLine}}.
where   : {token, {'where', TokenLine}}.
{C}     : {token, {comp, TokenLine, atom(TokenChars)}}.
'{S1}+' : S = strip(TokenChars, TokenLen),
	  {token, {string, TokenLine, bitstring(S)}}.
"{S2}+" : S = strip(TokenChars, TokenLen),
	  {token, {string, TokenLine, bitstring(S)}}.
{L}+    : {token, {name, TokenLine, bitstring(TokenChars)}}.
{D}+    : {token, {integer, TokenLine, integer(TokenChars)}}.
{P}     : {token, {atom(TokenChars), TokenLine}}.
{WS}+   : skip_token.

Erlang code.

atom(TokenChars) -> list_to_atom(TokenChars).
integer(TokenChars) -> list_to_integer(TokenChars).
bitstring(TokenChars) -> list_to_bitstring(TokenChars).
strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
