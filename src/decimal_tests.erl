-module(decimal_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    decimal:start().

cleanup(ok) ->
    decimal:stop().

all_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
        eval_list_nan(),
        eval_list_infinity(),
        eval_list_zero(),
        eval_list(),
        to_list(),
        basic_addition(),
        basic_subtraction(),
        basic_multiplication(),
        basic_division()
      ] }.
    

eval_list_nan() ->
    [
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("nan") ),
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("+nan") ),
      ?_assertMatch( {decimal, '-', nan}, decimal:eval("-nan") ),
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("NAN") ),
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("+NAN") ),
      ?_assertMatch( {decimal, '-', nan}, decimal:eval("-NAN") ),
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("Nan") ),
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("+Nan") ),
      ?_assertMatch( {decimal, '-', nan}, decimal:eval("-Nan") ),
      ?_assertMatch( {decimal, '+', nan}, decimal:eval("AnyThing") )
    ].
    
eval_list_infinity() ->
    [
      ?_assertMatch( {decimal, '+', infinity}, decimal:eval("infinity") ),
      ?_assertMatch( {decimal, '+', infinity}, decimal:eval("+infinity") ),
      ?_assertMatch( {decimal, '-', infinity}, decimal:eval("-infinity") ),
      ?_assertMatch( {decimal, '+', infinity}, decimal:eval("INFINITY") ),
      ?_assertMatch( {decimal, '+', infinity}, decimal:eval("+INFINITY") ),
      ?_assertMatch( {decimal, '-', infinity}, decimal:eval("-INFINITY") ),
      ?_assertMatch( {decimal, '+', infinity}, decimal:eval("Infinity") ),
      ?_assertMatch( {decimal, '+', infinity}, decimal:eval("+Infinity") ),
      ?_assertMatch( {decimal, '-', infinity}, decimal:eval("-Infinity") )
    ].
    
eval_list_zero() ->
    [
      ?_assertMatch( {decimal, '+', {<<0>>, 0}}, decimal:eval("0") ),
      ?_assertMatch( {decimal, '+', {<<0>>, 0}}, decimal:eval("+0") ),
      ?_assertMatch( {decimal, '-', {<<0>>, 0}}, decimal:eval("-0") ),
      ?_assertMatch( {decimal, '+', {<<0>>, 0}}, decimal:eval("0.0") ),
      ?_assertMatch( {decimal, '+', {<<0>>, 0}}, decimal:eval("+0.0") ),
      ?_assertMatch( {decimal, '-', {<<0>>, 0}}, decimal:eval("-0.0") ),
      ?_assertMatch( {decimal, '+', {<<0>>, 0}}, decimal:eval("0.0E1") ),
      ?_assertMatch( {decimal, '+', {<<0>>, 0}}, decimal:eval("+0.0E1") ),
      ?_assertMatch( {decimal, '-', {<<0>>, 0}}, decimal:eval("-0.0E1") )
    ].

eval_list() ->
    [
      ?_assertMatch( {decimal, '+', {<<1,2>>,      0}}, decimal:eval("12") ),
      ?_assertMatch( {decimal, '+', {<<1,2>>,      0}}, decimal:eval("+12") ),
      ?_assertMatch( {decimal, '-', {<<1,2>>,      0}}, decimal:eval("-12") ),
      ?_assertMatch( {decimal, '+', {<<1,2,3,4>>, -2}}, decimal:eval("12.34") ),
      ?_assertMatch( {decimal, '+', {<<1,2,3,4>>, -2}}, decimal:eval("+12.34") ),
      ?_assertMatch( {decimal, '-', {<<1,2,3,4>>, -2}}, decimal:eval("-12.34") ),
      ?_assertMatch( {decimal, '+', {<<1,2,3,4>>,  1}}, decimal:eval("12.34E3") ),
      ?_assertMatch( {decimal, '+', {<<1,2,3,4>>,  1}}, decimal:eval("+12.34E3") ),
      ?_assertMatch( {decimal, '-', {<<1,2,3,4>>,  1}}, decimal:eval("-12.34E3") ),
      ?_assertMatch( {decimal, '+', {<<1,2,3,4>>, -5}}, decimal:eval("12.34E-3") ),
      ?_assertMatch( {decimal, '+', {<<1,2,3,4>>, -5}}, decimal:eval("+12.34E-3") ),
      ?_assertMatch( {decimal, '-', {<<1,2,3,4>>, -5}}, decimal:eval("-12.34E-3") )
    ].

to_list() ->
    [
      ?_assertMatch(  "NaN",      decimal:to_list({decimal, '+', nan}) ),
      ?_assertMatch( "-NaN",      decimal:to_list({decimal, '-', nan}) ),
      ?_assertMatch(  "NaN",      decimal:to_list({decimal, '+', nan}, "-3") ),
      ?_assertMatch( "-NaN",      decimal:to_list({decimal, '-', nan}, "-3") ),
      ?_assertMatch(  "12",       decimal:to_list({decimal, '+', {<<1,2>>,        0}}) ),
      ?_assertMatch( "-12",       decimal:to_list({decimal, '-', {<<1,2>>,        0}}) ),
      ?_assertMatch(  "0.00012",  decimal:to_list({decimal, '+', {<<1,2>>,       -5}}) ),
      ?_assertMatch( "-0.00012",  decimal:to_list({decimal, '-', {<<1,2>>,       -5}}) ),
      ?_assertMatch(  "1.2E+4",   decimal:to_list({decimal, '+', {<<1,2>>,        3}}) ),
      ?_assertMatch( "-1.2E+4",   decimal:to_list({decimal, '-', {<<1,2>>,        3}}) ),
      ?_assertMatch(  "12000.00", decimal:to_list({decimal, '+', {<<1,2>>,        3}}, "-2") ),
      ?_assertMatch( "-12000.00", decimal:to_list({decimal, '-', {<<1,2>>,        3}}, "-2") ),
      ?_assertMatch(  "12.344",   decimal:to_list({decimal, '+', {<<1,2,3,4,4>>, -3}}) ),
      ?_assertMatch(  "12.345",   decimal:to_list({decimal, '+', {<<1,2,3,4,5>>, -3}}) ),
      ?_assertMatch(  "12.346",   decimal:to_list({decimal, '+', {<<1,2,3,4,6>>, -3}}) ),
      ?_assertMatch(  "12.34",    decimal:to_list({decimal, '+', {<<1,2,3,4,4>>, -3}}, "-2") ),
      ?_assertMatch(  "12.34",    decimal:to_list({decimal, '+', {<<1,2,3,4,5>>, -3}}, "-2") ),
      ?_assertMatch(  "12.35",    decimal:to_list({decimal, '+', {<<1,2,3,4,6>>, -3}}, "-2") ),
      ?_assertMatch( "-12.344",   decimal:to_list({decimal, '-', {<<1,2,3,4,4>>, -3}}) ),
      ?_assertMatch( "-12.345",   decimal:to_list({decimal, '-', {<<1,2,3,4,5>>, -3}}) ),
      ?_assertMatch( "-12.346",   decimal:to_list({decimal, '-', {<<1,2,3,4,6>>, -3}}) ),
      ?_assertMatch( "-12.34",    decimal:to_list({decimal, '-', {<<1,2,3,4,4>>, -3}}, "-2") ),
      ?_assertMatch( "-12.34",    decimal:to_list({decimal, '-', {<<1,2,3,4,5>>, -3}}, "-2") ),
      ?_assertMatch( "-12.35",    decimal:to_list({decimal, '-', {<<1,2,3,4,6>>, -3}}, "-2") )
    ].

basic_addition() ->
    [
      ?_assertMatch(  "3.5", decimal:to_list(decimal:add( "1.5",  "2")) ),
      ?_assertMatch(  "0.5", decimal:to_list(decimal:add("-1.5",  "2")) ),
      ?_assertMatch( "-0.5", decimal:to_list(decimal:add( "1.5", "-2")) ),
      ?_assertMatch( "-3.5", decimal:to_list(decimal:add("-1.5", "-2")) ),

      ?_assertMatch(  "4", decimal:to_list(decimal:add( "1.5",  "2.5")) ),
      ?_assertMatch(  "1", decimal:to_list(decimal:add("-1.5",  "2.5")) ),
      ?_assertMatch( "-1", decimal:to_list(decimal:add( "1.5", "-2.5")) ),
      ?_assertMatch( "-4", decimal:to_list(decimal:add("-1.5", "-2.5")) ),

      ?_assertMatch(  "3", decimal:to_list(decimal:add( "3",  "0")) ),
      ?_assertMatch( "-3", decimal:to_list(decimal:add("-3",  "0")) ),
      ?_assertMatch(  "3", decimal:to_list(decimal:add( "3", "-0")) ),
      ?_assertMatch( "-3", decimal:to_list(decimal:add("-3", "-0")) ),

      ?_assertMatch(  "0", decimal:to_list(decimal:add( "0",  "0")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:add("-0",  "0")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:add( "0", "-0")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:add("-0", "-0")) ),

      ?_assertMatch(  "Infinity", decimal:to_list(decimal:add( "Infinity",  "Infinity")) ),
      ?_assertMatch(  "NaN",      decimal:to_list(decimal:add("-Infinity",  "Infinity")) ),
      ?_assertMatch(  "NaN",      decimal:to_list(decimal:add( "Infinity", "-Infinity")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:add("-Infinity", "-Infinity")) ),
     
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:add( "Infinity",  "0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:add("-Infinity",  "0")) ),
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:add( "Infinity", "-0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:add("-Infinity", "-0")) ),

      ?_assertMatch(  "NaN", decimal:to_list(decimal:add( "NaN",  "0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:add("-NaN",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:add( "NaN", "-0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:add("-NaN", "-0")) )
    ].

basic_subtraction() ->
    [
      ?_assertMatch( "-0.5", decimal:to_list(decimal:subtract( "1.5",  "2")) ),
      ?_assertMatch( "-3.5", decimal:to_list(decimal:subtract("-1.5",  "2")) ),
      ?_assertMatch(  "3.5", decimal:to_list(decimal:subtract( "1.5", "-2")) ),
      ?_assertMatch(  "0.5", decimal:to_list(decimal:subtract("-1.5", "-2")) ),

      ?_assertMatch( "-1", decimal:to_list(decimal:subtract( "1.5",  "2.5")) ),
      ?_assertMatch( "-4", decimal:to_list(decimal:subtract("-1.5",  "2.5")) ),
      ?_assertMatch(  "4", decimal:to_list(decimal:subtract( "1.5", "-2.5")) ),
      ?_assertMatch(  "1", decimal:to_list(decimal:subtract("-1.5", "-2.5")) ),

      ?_assertMatch(  "3", decimal:to_list(decimal:subtract( "3",  "0")) ),
      ?_assertMatch( "-3", decimal:to_list(decimal:subtract("-3",  "0")) ),
      ?_assertMatch(  "3", decimal:to_list(decimal:subtract( "3", "-0")) ),
      ?_assertMatch( "-3", decimal:to_list(decimal:subtract("-3", "-0")) ),

      ?_assertMatch(  "0", decimal:to_list(decimal:subtract( "0",  "0")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:subtract("-0",  "0")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:subtract( "0", "-0")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:subtract("-0", "-0")) ),

      ?_assertMatch(  "NaN",      decimal:to_list(decimal:subtract( "Infinity",  "Infinity")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:subtract("-Infinity",  "Infinity")) ),
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:subtract( "Infinity", "-Infinity")) ),
      ?_assertMatch(  "NaN",      decimal:to_list(decimal:subtract("-Infinity", "-Infinity")) ),
     
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:subtract( "Infinity",  "0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:subtract("-Infinity",  "0")) ),
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:subtract( "Infinity", "-0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:subtract("-Infinity", "-0")) ),

      ?_assertMatch(  "NaN", decimal:to_list(decimal:subtract( "NaN",  "0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:subtract("-NaN",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:subtract( "NaN", "-0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:subtract("-NaN", "-0")) )
    ].

basic_multiplication() ->
    [
      ?_assertMatch(  "3", decimal:to_list(decimal:multiply( "1.5",  "2")) ),
      ?_assertMatch( "-3", decimal:to_list(decimal:multiply("-1.5",  "2")) ),
      ?_assertMatch( "-3", decimal:to_list(decimal:multiply( "1.5", "-2")) ),
      ?_assertMatch(  "3", decimal:to_list(decimal:multiply("-1.5", "-2")) ),

      ?_assertMatch(  "2", decimal:to_list(decimal:multiply( "0.6666666666666666666666666666666667",  "3")) ),
      ?_assertMatch( "-2", decimal:to_list(decimal:multiply("-0.6666666666666666666666666666666667",  "3")) ),
      ?_assertMatch( "-2", decimal:to_list(decimal:multiply( "0.6666666666666666666666666666666667", "-3")) ),
      ?_assertMatch(  "2", decimal:to_list(decimal:multiply("-0.6666666666666666666666666666666667", "-3")) ),

      ?_assertMatch(  "0", decimal:to_list(decimal:multiply( "3",  "0")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:multiply("-3",  "0")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:multiply( "3", "-0")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:multiply("-3", "-0")) ),

      ?_assertMatch(  "0", decimal:to_list(decimal:multiply( "0",  "0")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:multiply("-0",  "0")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:multiply( "0", "-0")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:multiply("-0", "-0")) ),

      ?_assertMatch(  "Infinity", decimal:to_list(decimal:multiply( "Infinity",  "Infinity")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:multiply("-Infinity",  "Infinity")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:multiply( "Infinity", "-Infinity")) ),
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:multiply("-Infinity", "-Infinity")) ),
     
      ?_assertMatch(  "NaN", decimal:to_list(decimal:multiply( "Infinity",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:multiply("-Infinity",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:multiply( "Infinity", "-0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:multiply("-Infinity", "-0")) ),

      ?_assertMatch(  "NaN", decimal:to_list(decimal:multiply( "NaN",  "0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:multiply("-NaN",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:multiply( "NaN", "-0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:multiply("-NaN", "-0")) )
    ].

basic_division() ->
    [
      ?_assertMatch(  "1.5", decimal:to_list(decimal:divide( "3",  "2")) ),
      ?_assertMatch( "-1.5", decimal:to_list(decimal:divide("-3",  "2")) ),
      ?_assertMatch( "-1.5", decimal:to_list(decimal:divide( "3", "-2")) ),
      ?_assertMatch(  "1.5", decimal:to_list(decimal:divide("-3", "-2")) ),

      ?_assertMatch(  "0.6666666666666666666666666666666667", decimal:to_list(decimal:divide( "2",  "3")) ),
      ?_assertMatch( "-0.6666666666666666666666666666666667", decimal:to_list(decimal:divide("-2",  "3")) ),
      ?_assertMatch( "-0.6666666666666666666666666666666667", decimal:to_list(decimal:divide( "2", "-3")) ),
      ?_assertMatch(  "0.6666666666666666666666666666666667", decimal:to_list(decimal:divide("-2", "-3")) ),

      ?_assertMatch(  "Infinity", decimal:to_list(decimal:divide( "3",  "0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:divide("-3",  "0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:divide( "3", "-0")) ),
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:divide("-3", "-0")) ),

      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide( "0",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide("-0",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide( "0", "-0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide("-0", "-0")) ),

      ?_assertMatch(  "0", decimal:to_list(decimal:divide( "0",  "Infinity")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:divide("-0",  "Infinity")) ),
      ?_assertMatch( "-0", decimal:to_list(decimal:divide( "0", "-Infinity")) ),
      ?_assertMatch(  "0", decimal:to_list(decimal:divide("-0", "-Infinity")) ),
     
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide( "Infinity",  "Infinity")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide("-Infinity",  "Infinity")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide( "Infinity", "-Infinity")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide("-Infinity", "-Infinity")) ),
     
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:divide( "Infinity",  "0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:divide("-Infinity",  "0")) ),
      ?_assertMatch( "-Infinity", decimal:to_list(decimal:divide( "Infinity", "-0")) ),
      ?_assertMatch(  "Infinity", decimal:to_list(decimal:divide("-Infinity", "-0")) ),

      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide( "NaN",  "0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:divide("-NaN",  "0")) ),
      ?_assertMatch(  "NaN", decimal:to_list(decimal:divide( "NaN", "-0")) ),
      ?_assertMatch( "-NaN", decimal:to_list(decimal:divide("-NaN", "-0")) )
    ].


