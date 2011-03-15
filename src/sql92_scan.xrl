%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc A grammar of a subset of SQL-92

Definitions.

D   = [0-9]
L   = [A-Za-z_][A-Za-z0-9_.]*
WS  = ([\000-\s]|%.*)
C   = (<|<=|>=|>|<>)
P   = [-+*/(),;=]

Rules.

{C}     : {token, {comp, TokenLine, atom(TokenChars)}}.
{D}+    : {token, {integer, TokenLine, integer(TokenChars)}}.
{P}     : {token, {atom(TokenChars), TokenLine}}.
'(\\\^.|\\.|[^'])*' : S = string_gen(strip(TokenChars, TokenLen)),
 		      {token, {string, TokenLine, bitstring(S)}}.
"(\\\^.|\\.|[^"])*" : S = string_gen(strip(TokenChars, TokenLen)),
 		      {token, {string, TokenLine, bitstring(S)}}.
{L}+    : R = atom(string_gen(TokenChars)),
	  case reserved_word(R) of
	       true -> {token, {R, TokenLine}};
	       false -> {token, {name, TokenLine, bitstring(TokenChars)}}
          end.
{WS}+   : skip_token.

Erlang code.

atom(TokenChars) -> list_to_atom(TokenChars).
integer(TokenChars) -> list_to_integer(TokenChars).
bitstring(TokenChars) -> list_to_bitstring(TokenChars).
strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).

reserved_word('all') -> true;
reserved_word('and') -> true;
reserved_word('asc') -> true;
reserved_word('begin') -> true;
reserved_word('between') -> true;
reserved_word('by') -> true;
reserved_word('commit') -> true;
reserved_word('count') -> true;
reserved_word('delete') -> true;
reserved_word('desc') -> true;
reserved_word('distinct') -> true;
reserved_word('from') -> true;
reserved_word('group') -> true;
reserved_word('having') -> true;
reserved_word('insert') -> true;
reserved_word('in') -> true;
reserved_word('into') -> true;
reserved_word('is') -> true;
reserved_word('like') -> true;
reserved_word('limit') -> true;
reserved_word('not') -> true;
reserved_word('null') -> true;
reserved_word('offset') -> true;
reserved_word('or') -> true;
reserved_word('order') -> true;
reserved_word('rollback') -> true;
reserved_word('select') -> true;
reserved_word('set') -> true;
reserved_word('update') -> true;
reserved_word('values') -> true;
reserved_word('where') -> true;
reserved_word(_) -> false.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;
escape_char($r) -> $\r;
escape_char($t) -> $\t;
escape_char($v) -> $\v;
escape_char($b) -> $\b;
escape_char($f) -> $\f;
escape_char($e) -> $\e;
escape_char($s) -> $\s;
escape_char($d) -> $\d;
escape_char(C) -> C.
