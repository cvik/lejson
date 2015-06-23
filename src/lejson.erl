%% Lightweight erlang json encode/decode library
%%
%% Due to the internal ordering of erlang maps,
%%     this hold: Term == json:decode(json:encode(Term))
%%     but this doesn't: Json == json:encode(json:decode(Json))
%% The new json produced will be equivalent with the old, except
%% for the ordering of object members.
%%
%% TODO: Add options to decode that saves the ordering in a special key and
%%       ensures that if encode finds this special key, it will enforce
%%       that order.
%% TODO: Add options to return iolist instead of binary on encode/1
%% TODO: Add options to convert keys to atoms on decode/1
%% TODO: More strict number parsing.
%% ----------------------------------------------------------------------------

-module(lejson).

-copyright('Christoffer Vikström <chvi77@gmail.com>').

-export([encode/1, decode/1]).
-export([scan/1]).

%% Encode ---------------------------------------------------------------------

-spec encode([] | map()) -> binary().
encode(Array) when is_list(Array) ->
    iolist_to_binary(encode_array(Array));
encode(Map) when is_map(Map) ->
    iolist_to_binary(encode_map(Map)).

encode_array(Array) when is_list(Array) ->
    ["[", string:join([ encode_value(V) || V <- Array ], ", "), "]"].

encode_map(Map) when is_map(Map) ->
    Members = maps:to_list(Map),
    ["{", string:join([ ["\"", encode_key(Key), "\"",
                         ": ", encode_value(Value)] ||
                        {Key, Value} <- Members ], ","), "}"].

encode_value(true) -> "true";
encode_value(false) -> "false";
encode_value(null) -> "null";
encode_value({{_,_,_}, {_,_,_}} = Dt) -> encode_datetime(Dt);
encode_value(Atom) when is_atom(Atom) -> [$", atom_to_list(Atom), $"];
encode_value(Int) when is_integer(Int) -> integer_to_list(Int);
encode_value(Float) when is_float(Float) -> float_to_list(Float);
encode_value(Bin) when is_binary(Bin) -> [$", encode_string(Bin), $"];
encode_value(#{} = Map) -> encode_map(Map);
encode_value(Array) when is_list(Array) -> encode_array(Array).

encode_key(Key) when is_atom(Key) -> erlang:atom_to_binary(Key, utf8);
encode_key(Key) -> Key.

encode_string(Bin) when is_binary(Bin) ->
    iolist_to_binary(encode_string(binary_to_list(Bin), [])).

encode_string([$"|Rest], Res) -> encode_string(Rest, [$", $\\|Res]);
encode_string([$\\|Rest], Res) -> encode_string(Rest, [$\\, $\\|Res]);
encode_string([$/|Rest], Res) -> encode_string(Rest, [$/, $\\|Res]);
encode_string([$\b|Rest], Res) -> encode_string(Rest, [$b, $\\|Res]);
encode_string([$\f|Rest], Res) -> encode_string(Rest, [$f, $\\|Res]);
encode_string([$\n|Rest], Res) -> encode_string(Rest, [$n, $\\|Res]);
encode_string([$\r|Rest], Res) -> encode_string(Rest, [$r, $\\|Res]);
encode_string([$\t|Rest], Res) -> encode_string(Rest, [$t, $\\|Res]);
encode_string([C|Rest], Res) -> encode_string(Rest, [C|Res]);
encode_string([], Res) -> lists:reverse(Res).

encode_datetime({{H,M,D},{Hh,Mm,Ss}}) ->
    TsStr = io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                          [H, M, D, Hh, Mm, Ss]),
    ["\"", TsStr, "\""].

%% Decode ---------------------------------------------------------------------

-spec decode(iolist()) -> list() | map() | {error, not_json}.
decode(Str) when is_binary(Str) ->
    decode(binary_to_list(Str));
decode(Str) ->
    case is_json(Str) of
        true ->
            Tokens = scan(Str),
            parse(Tokens);
        false ->
            {error, not_json}
    end.

scan(Str) ->
    scan(Str, []).

scan([${|Rest], Res) ->
    scan(Rest, [begin_object|Res]);
scan([$}|Rest], Res) ->
    scan(Rest, [end_object|Res]);
scan([$:|Rest], [{value,Str}|Res]) when is_binary(Str) ->
    scan(Rest, [key_delimiter, {key, Str}|Res]);
scan([$[|Rest], Res) ->
    scan(Rest, [begin_array|Res]);
scan([$]|Rest], Res) ->
    scan(Rest, [end_array|Res]);
scan([$"|Rest], Res) ->
    {String, NewRest} = scan_string(Rest),
    scan(NewRest, [{value, String}|Res]);
scan([$,|Rest], Res) ->
    scan(Rest, [comma|Res]);
scan([$t,$r,$u,$e|Rest], Res) ->
    scan(Rest, [{value, true}|Res]);
scan([$f,$a,$l,$s,$e|Rest], Res) ->
    scan(Rest, [{value, false}|Res]);
scan([$n,$u,$l,$l|Rest], Res) ->
    scan(Rest, [{value, null}|Res]);
scan([C|_]=Rest, Res) when C >= $0, C =< $9; C == $- ->
    {Num, NewRest} = scan_number(Rest),
    scan(NewRest, [{value, Num}|Res]);
scan([$ |Rest], Res) ->
    scan(Rest, Res);
scan([$\n|Rest], Res) ->
    scan(Rest, Res);
scan([$\t|Rest], Res) ->
    scan(Rest, Res);
scan([$\r|Rest], Res) ->
    scan(Rest, Res);
scan([C|Rest], Res) ->
    scan(Rest, [C|Res]);
scan([], Res) ->
    lists:reverse(Res).

scan_string(String) ->
    scan_string(String, []).

scan_string([$\\, $"|Rest], Res) ->
    scan_string(Rest, [$\"|Res]);
scan_string([$\\, $\\|Rest], Res) ->
    scan_string(Rest, [$\\|Res]);
scan_string([$\\, $/|Rest], Res) ->
    scan_string(Rest, [$/|Res]);
scan_string([$\\, $b|Rest], Res) ->
    scan_string(Rest, [$\b|Res]);
scan_string([$\\, $f|Rest], Res) ->
    scan_string(Rest, [$\f|Res]);
scan_string([$\\, $n|Rest], Res) ->
    scan_string(Rest, [$\n|Res]);
scan_string([$\\, $r|Rest], Res) ->
    scan_string(Rest, [$\r|Res]);
scan_string([$\\, $t|Rest], Res) ->
    scan_string(Rest, [$\t|Res]);
scan_string([$"|Rest], Res) ->
    {list_to_binary(lists:reverse(Res)), Rest};
scan_string([$\\,$u,A,B,C,D|Rest], Res) ->
    scan_string(Rest, [hex(A,B,C,D)|Res]);
scan_string([C|Rest], Res) ->
    scan_string(Rest, [C|Res]);
scan_string([], Res) ->
    {error,  {no_end_of_string, Res}}.

%% TODO: This is flawed, might let malformed floats through
scan_number([$-,C|Rest]) when C >= $0, C =< $9 ->
    {NumStr, NewRest} = scan_number([C|Rest], []),
    {to_num([$-|NumStr]), NewRest};
scan_number([C|Rest]) when C >= $0, C =< $9 ->
    {NumStr, NewRest} = scan_number([C|Rest], []),
    {to_num(NumStr), NewRest}.

scan_number([C|Rest], Res) when C >= $0, C =< $9;
                                C == $-; C == $.;
                                C == $e; C == $E;
                                C == $+; C == $- ->
    scan_number(Rest, [C|Res]);
scan_number([_|_]=Rest, Res) ->
    {lists:reverse(Res), Rest};
scan_number([], Res) ->
    {error,  {no_end_of_num, Res}}.

to_num(Str) ->
    case lists:member($., Str) of
        true ->
            list_to_float(Str);
        false ->
            list_to_integer(Str)
    end.

parse([begin_object|Rest]) ->
    {Object, _} = parse_object(Rest, #{}),
    Object;
parse([begin_array|Rest]) ->
    {Array, _} = parse_array(Rest, []),
    Array.

parse_array([{value,Val}, comma|Rest], Res) ->
    parse_array(Rest, [Val|Res]);
parse_array([{value,Val}, end_array|Rest], Res) ->
    {lists:reverse([Val|Res]), Rest};
parse_array([begin_object|Rest], Res) ->
    case parse_object(Rest, #{}) of
        {Obj, [comma|NewRest]} ->
            parse_array(NewRest, [Obj|Res]);
        {Obj, [end_array|NewRest]} ->
            {lists:reverse([Obj|Res]), NewRest}
    end;
parse_array([begin_array|Rest], Res) ->
    case parse_array(Rest, []) of
        {Array, [comma|NewRest]} ->
            parse_array(NewRest, [Array|Res]);
        {Array, [end_array|NewRest]} ->
            {lists:reverse([Array|Res]), NewRest}
    end;
parse_array([end_array|Rest], Res) ->
    {lists:reverse(Res), Rest}.

parse_object([{key,Key},key_delimiter,{value,Val},comma|Rest], Map) ->
    parse_object(Rest, maps:put(Key, Val, Map));
parse_object([{key,Key},key_delimiter,{value,Val},end_object|Rest], Map) ->
    {maps:put(Key, Val, Map), Rest};
parse_object([{key,Key},key_delimiter,begin_object|Rest], Map) ->
    case parse_object(Rest, #{}) of
        {Obj, [comma|NewRest]} ->
            parse_object(NewRest, maps:put(Key, Obj, Map));
        {Obj, [end_object|NewRest]} ->
            {maps:put(Key, Obj, Map), NewRest}
    end;
parse_object([{key, Key}, key_delimiter, begin_array|Rest], Map) ->
    case parse_array(Rest, []) of
        {Array, [comma|NewRest]} ->
            parse_object(NewRest, maps:put(Key, Array, Map));
        {Array, [end_object|NewRest]} ->
            {maps:put(Key, Array, Map), NewRest}
    end;
parse_object([end_object|Rest], Map) ->
    {Map, Rest}.

hex(A,B,C,D) ->
    [16*hex_char(A)+hex_char(B), 16*hex_char(C)+hex_char(D)].

hex_char(C) when C >= $a, C =< $f -> 10+C-$a;
hex_char(C) when C >= $A, C =< $F -> 10+C-$A;
hex_char(N) when N >= $0, N =< $9 -> N-$0.

is_json(<<${, _/binary>>) -> true;
is_json(<<$[, _/binary>>) -> true;
is_json([${ | _]) -> true;
is_json([$[ | _]) -> true;
is_json(_) -> false.
