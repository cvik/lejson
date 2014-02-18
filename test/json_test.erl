-module(json_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

config_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     {inorder, [ {timeout, 120,
                  {atom_to_list(Name),
                   fun() -> test_decode(Data) end}} ||
                  {Name,_,_} = Data <- json_strings_should_pass()]}}.

setup_config() -> ok.
cleanup_config(_) -> ok.

json_strings_should_pass() ->
    [{empty_object, <<"{}">>, #{}},
     {simple_object_string_value,
      <<"{\"V\": \"1\"}">>,
      #{<<"V">> => <<"1">>}},
     {space_tester,
      <<"{  \"v\":\"1\"\r\n}">>,
      #{<<"v">> => <<"1">>}},
     {simple_object_int_value,
      <<"{\"V\": 1}">>,
      #{<<"V">> => 1}},
     {simple_object_quote_in_string,
      <<"{ \"v\":\"ab'c\"}">>,
      #{<<"v">> => <<"ab'c">>}},
     {simple_object_float_value,
      <<"{ \"PI\":3.141E-10}">>,
      #{<<"PI">> => 3.141e-10}},
     {lower_case_float_value,
      <<"{ \"PI\":3.141e-10}">>,
      #{<<"PI">> => 3.141e-10}},
     {long_number,
      <<"{ \"v\":12345123456789}">>,
      #{<<"v">> => 12345123456789}},
     {bigint_number,
      <<"{ \"v\":123456789123456789123456789}">>,
      #{<<"v">> => 123456789123456789123456789}},
     {simple_digit_array,
      <<"[ 1,2,3,4]">>,
      [1,2,3,4]},
     {simple_string_array,
      <<"[ \"1\",\"2\",\"3\",\"4\"]">>,
      [<<"1">>,<<"2">>,<<"3">>,<<"4">>]},
     {array_of_empty_objects,
      <<"[ { }, { },[]]">>,
      [#{}, #{}, []]},
     {lowercase_unicode_text,
      <<"{ \"v\":\"\u042e\"}">>,
      #{<<"v">> => <<"Ю">>}},
     {uppercase_unicode_text,
      <<"{ \"v\":\"\u042E\"}">>,
      #{<<"v">> => <<"Ю">>}},
     {non_protected_text,
      <<"{ \"a\":\"hp://foo\"}">>,
      #{<<"a">> => <<"hp://foo">>}},
     {null,
      <<"{ \"a\":null}">>,
      #{<<"a">> => null}},
     {boolean,
      <<"{ \"a\":true}">>,
      #{<<"a">> => true}},
     {non_trimmed_data,
      <<"{ \"a\" : false }">>,
      #{<<"a">> => false}},
     {double_precision_float,
      <<"{ \"v\":1.7976931348623157E308}">>,
      #{<<"v">> => 1.7976931348623157e308}}].

json_strings_should_fail() ->
    [{truncated_value, "{\"X\":\"s"},
     {truncated_key, "{\"X"}].

test_decode({_Type, Data, Expected}) ->
    ?assertEqual(Expected, json:decode(Data)).
