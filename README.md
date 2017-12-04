## Le'Json - Lightweight Erlang Json library

Requires R17 since it produces maps on decode. Not yet handling unicode
code points like "\\uD8B3" (arabic) correctly. Other than that it seem
to keep with the standard (rfc4627).

## Exports

```erlang
-spec lejson:decode(binary()) -> list() | map() | {error, not_json}.
-spec lejson:encode(list() | map()) -> binary().
```

## Example usage

```erlang
1> l(lejson).
ok
2> M = lejson:decode(<<"{\"boolean\": [true, false],
                         \"neg_num\": -12,
                         \"floats\": [-22.3, -22.3e-12, 22.3E-12,
                                      22.3E+4, 22.3E+4, 22.3E4],
                         \"null\": null,
                         \"pos_int\": 6789,
                         \"string_value\": \"value\",
                         \"utf_value\": \"\\uC3B8 and \\uc2a9\",
                         \"arabic\": \"\\uD8B3\\ud8b5\\ud8b8\",
                         \"more unicode\": \" \\uD834 \\uDD1E \",
                         \"array\": [{\"object_inside_array\": 1}],
                         \"nested_array\": [[[79]]],
                         \"another_array\": [1,2,3,[1,[2],3],12]}">>).
#{<<"another_array">> => [1,2,3,[1,[2],3],12],
  <<"arabic">> => <<216,179,216,181,216,184>>,
  <<"array">> => [#{<<"object_inside_array">> => 1}],
  <<"boolean">> => [true,false],
  <<"floats">> => [-22.3,-2.23e-11,2.23e-11,2.23e5,2.23e5,2.23e5],
  <<"more unicode">> => <<32,216,52,32,221,30,32>>,
  <<"neg_num">> => -12,
  <<"nested_array">> => [["O"]],
  <<"null">> => null,
  <<"pos_int">> => 6789,
  <<"string_value">> => <<"value">>,
  <<"utf_value">> => <<"ø and ©"/utf8>>}
3> lejson:encode(M)
<<"{\"another_array\": [1, 2, 3, [1, [2], 3], 12],\"arabic\": \""...>>
```

## License

Apache license version 2.0. See the LICENSE file for details.
