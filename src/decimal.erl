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
-module(decimal).

%-record(decimal, { sign = '+',
%	           magnitude= {<<0>>,0} } ).

%% API
-export([start/0, stop/0]).
-export([info_lib/0, eval/1]).
-export([to_list/1, to_list/2]).
%-export([to_packed/1]).
-export([abs/1, negate/1, exp/1, log10/1, ln/1, sqrt/1]).
-export([add/2, subtract/2, multiply/2, divide/2, power/2]).
-export([min/2, max/2, minmag/2, maxmag/2]).

-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called to start the decimal application.
%% It initializes the elacs application as well as all dependant 
%% applications.
%%
%% @spec start() -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% This function is called to stop the decimal application.
%% It stops erlacs and all depenancies.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    application:stop(?MODULE).

info_lib() ->
    decimal_raw:info_lib().

eval({decimal, _}=D) ->
    D;
eval(V) ->
    decimal_raw:to_decimal(V).

to_list(V, Scale) ->
    decimal_raw:to_list(V, Scale).

to_list(V) ->
    decimal_raw:to_list(V).

%to_float(V) ->
%    decimal_raw:to_float(V).

%to_packed(V) ->
%    decimal_raw:to_packed(V).

abs(V) ->
    eval(decimal_raw:abs(V)).

negate(V) ->
    eval(decimal_raw:negate(V)).

exp(V) ->
    eval(decimal_raw:exp(V)).

log10(V) ->
    eval(decimal_raw:log10(V)).

ln(V) ->
    eval(decimal_raw:ln(V)).

sqrt(V) ->
    eval(decimal_raw:sqrt(V)).

add(V1, V2) ->
    eval(decimal_raw:add(V1, V2)).

subtract(V1, V2) ->
    eval(decimal_raw:subtract(V1, V2)).

multiply(V1, V2) ->
    eval(decimal_raw:multiply(V1, V2)).

divide(V1, V2) ->
    eval(decimal_raw:divide(V1, V2)).

power(V1, V2) ->
    eval(decimal_raw:power(V1, V2)).

min(V1, V2) ->
    eval(decimal_raw:min(V1, V2)).

max(V1, V2) ->
    eval(decimal_raw:max(V1, V2)).

minmag(V1, V2) ->
    eval(decimal_raw:minmag(V1, V2)).

maxmag(V1, V2) ->
    eval(decimal_raw:maxmag(V1, V2)).

%%%===================================================================
%%% The parser transform
%%%===================================================================


%% parse transform -- this is not even close to working -- don't use it
parse_transform(Forms, Options) ->
    io:format("transforming...~n", []),
    ct_expand:function(
               {?MODULE, eval, 1},
               fun(Node, _Context) ->
	           io:format("expanding...~n", []),
       	           io:format("input form:~n~p~n", [Node]),
       	           io:format("revert form:~n~p~n", [erl_syntax:revert(Node)]),
	       	   Result = case erl_syntax:application_arguments(Node) of
	       	                [Expr] ->
				    Pos = erl_syntax:get_pos(Node),
				    erl_syntax:set_pos(
       			               ct_application(decimal, eval,
					     	      [ct_xform(Expr)], Pos),
				       Pos);
	       	                _Args ->
	       		            erlang:error(illegal_form)
	       	             end,
		   io:format("result:~n~p~n", [Result]),
	       	   Result
               end,
	       Forms,
       	       Options).


ct_xform({string, Pos, _}=Str) ->
    ct_application(decimal_raw, eval, [Str], Pos);
ct_xform({float, Pos, Float}) ->
    ct_application(decimal_raw, eval,
	           [{string,Pos,float_to_list(Float)}], Pos);
ct_xform({integer, Pos, Integer}) ->
    ct_application(decimal_raw, eval,
        	   [{string,Pos,integer_to_list(Integer)}], Pos);
ct_xform({var, Pos, _}=Var) ->
    ct_application(decimal_raw, eval, [Var], Pos);
ct_xform(Expr) ->
   io:format("Unhandled expr xform:~n~p~n", [Expr]).

ct_application(M, F, A, Pos) ->
    erl_syntax:application({atom,Pos,M}, {atom,Pos,F}, A).

