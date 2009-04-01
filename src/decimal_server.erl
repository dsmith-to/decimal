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
-module(decimal_server).

-behaviour(gen_server).

%% API
-export([start_link/0,client_port/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-compile({inline,[{port_names,0}]}).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

client_port() ->
    element(erlang:system_info(scheduler_id) rem size(port_names()) + 1,
	    port_names()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    erl_ddll:start(),
    PrivDir = code:priv_dir(decimal),
    LibDir1 = filename:join([PrivDir, "lib"]),
    case erl_ddll:load_driver(LibDir1, decimal_drv) of
        ok -> ok;
        {error,_} ->
            LibDir2 = filename:join(LibDir1, 
                                    erlang:system_info(system_architecture)),
	    erl_ddll:load_driver(LibDir2, decimal_drv)
    end,

    open_ports(decimal_drv,size(port_names())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    {noreply, State};
handle_info({'EXIT', Port, Reason}, State) when is_port(Port) ->
    {stop, {port_died, Reason}, State};
handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    close_ports(size(port_names())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open_ports(_,0) ->
    {ok, []};
open_ports(Cmd,N) ->   
    Port = open_port({spawn, Cmd}, []),
    %% check that driver is loaded, linked and working
    %% since decimal_drv links towards libdecimal, this is a good thing
    %% since libdecimal is known to be bad with backwards compatibility
    case catch port_control(Port, 0, []) of
	{'EXIT', _} ->
	    {stop, nodriver};
	_ ->
	    register(element(N,port_names()), Port),
	    open_ports(Cmd,N-1)
    end.

close_ports(0) ->
    ok;
close_ports(N) ->   
    element(N,port_names()) ! {self(), close},
    close_ports(N-1).

port_names() -> 
    { decimal_drv01, decimal_drv02, decimal_drv03, decimal_drv04,
      decimal_drv05, decimal_drv06, decimal_drv07, decimal_drv08,
      decimal_drv09, decimal_drv10, decimal_drv11, decimal_drv12,
      decimal_drv13, decimal_drv14, decimal_drv15, decimal_drv16 }.
