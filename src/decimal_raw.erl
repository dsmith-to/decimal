%%%-------------------------------------------------------------------
%%% @author David B. Smith <dave.smith.to@gmail.com>
%%% @copyright (C) 2009, David Smith
%%% @doc
%%%
%%% @end
%%% Created : 25 Oct 2008
%%%
%%% Copyright (c) 2009 David B. Smith
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 3. Neither the name of the copyright holder nor the names of contributors
%%%    may be used to endorse or promote products derived from this software
%%%    without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(decimal_raw).

-compile(export_all). 

%% API
-export([info_lib/0, eval/1]).
-export([to_list/1, to_list/2, to_decimal/1]).
%-export([to_packed/1]).
-export([abs/1, negate/1, exp/1, log10/1, ln/1, sqrt/1]).
-export([add/2, subtract/2, multiply/2, divide/2, power/2]).
-export([min/2, max/2, minmag/2, maxmag/2]).

-define(INFO_LIB,    0).
%%conversions
-define(FROM_LIST,   1).
-define(TO_LIST,     2).
-define(FROM_FLOAT,  3).
-define(TO_FLOAT,    4).
-define(FROM_PACKED, 5).
-define(TO_PACKED,   6).
-define(FROM_BCD,    7).
-define(TO_BCD,      8).
%%uniary ops
-define(ABS,         9).
-define(MINUS,      10).
-define(EXP,        11).
-define(LOG10,      12).
-define(LN,         13).
-define(SQRT,       14).
%%binary ops
-define(ADD,        15).
-define(SUBTRACT,   16).
-define(MULTIPLY,   17).
-define(DIVIDE,     18).
-define(POWER,      19).
-define(MIN,        20).
-define(MAX,        21).
-define(MINMAG,     22).
-define(MAXMAG,     23).

info_lib() ->
    binary_to_list(control(?INFO_LIB, [])).

eval({decimal, '+', nan}) ->
    control(?FROM_BCD, <<0,0,0,0,32,0>>);
eval({decimal, '-', nan}) ->
    control(?FROM_BCD, <<0,0,0,0,160,0>>);
eval({decimal, '+', infinity}) ->
    control(?FROM_BCD, <<0,0,0,0,64,0>>);
eval({decimal, '-', infinity}) ->
    control(?FROM_BCD, <<0,0,0,0,192,0>>);
eval({decimal, '+', {Signif, Scale}}) ->
    control(?FROM_BCD, <<Scale:32/signed-native, 0, Signif/binary>>);
eval({decimal, '-', {Signif, Scale}}) ->
    control(?FROM_BCD, <<Scale:32/signed-native, 128, Signif/binary>>);
%eval({decpack, DP}) ->
%    control(?FROM_PACKED, DP);
eval(L) when is_list(L) ->
    control(?FROM_LIST, L);
eval(N) when is_number(N) ->
    control(?FROM_FLOAT, float(N));
eval(DR) when is_binary(DR) ->
    DR.

to_list(V, Scale)->
    binary_to_list(control_binop(?TO_LIST, V, Scale)).

to_list(V) ->
    binary_to_list(control_unop(?TO_LIST, V)).

%to_float(V) ->
%    binary_to_float(control_unop(?TO_FLOAT, V)).

%to_packed(V) ->
%    {decpack, control_unop(?TO_PACKED, V)}.

to_decimal(V) ->
    <<Scale:32/signed-native, Flags:1/binary, Signif/binary>>
        = control_unop(?TO_BCD, V),
    case Flags of
       <<0>>   -> {decimal, '+', {Signif, Scale}};
       <<128>> -> {decimal, '-', {Signif, Scale}};
       <<64>>  -> {decimal, '+', infinity};
       <<192>> -> {decimal, '-', infinity};
       <<0:1, _:7>> -> {decimal, '+', nan};
       <<1:1, _:7>> -> {decimal, '-', nan}
    end.
%%    {decimal, control_unop(?TO_BCD, V)}.

abs(V) ->
    control_unop(?ABS, V).

negate(V) ->
    control_unop(?MINUS, V).

exp(V) ->
    control_unop(?EXP, V).

log10(V) ->
    control_unop(?LOG10, V).

ln(V) ->
    control_unop(?LN, V).

sqrt(V) ->
    control_unop(?SQRT, V).

add(V1, V2) ->
    control_binop(?ADD, V1, V2).

subtract(V1, V2) ->
    control_binop(?SUBTRACT, V1, V2).

multiply(V1, V2) ->
    control_binop(?MULTIPLY, V1, V2).

divide(V1, V2) ->
    control_binop(?DIVIDE, V1, V2).

power(V1, V2) ->
    control_binop(?POWER, V1, V2).

min(V1, V2) ->
    control_binop(?MIN, V1, V2).

max(V1, V2) ->
    control_binop(?MAX, V1, V2).

minmag(V1, V2) ->
    control_binop(?MINMAG, V1, V2).

maxmag(V1, V2) ->
    control_binop(?MAXMAG, V1, V2).

%%%===================================================================
%%% Internal functions
%%%===================================================================

control_binop(Cmd, D1, D2) when is_binary(D1), is_binary(D2) ->
    control(Cmd, <<D1/binary,D2/binary>>);
control_binop(Cmd, V1, V2) ->
    control_binop(Cmd, eval(V1), eval(V2)).

control_unop(Cmd, D) when is_binary(D) ->
    control(Cmd, D);
control_unop(Cmd, V) ->
    control_unop(Cmd, eval(V)).

control(Cmd, Data) ->
    Port = decimal_server:client_port(),
    erlang:port_control(Port, Cmd, Data).
