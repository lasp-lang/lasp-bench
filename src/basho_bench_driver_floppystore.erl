%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2010 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_floppystore).

-export([new/1,
         floppy_valgen/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 1000).
-record(state, {node}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure bitcask is available
    case code:which(floppy) of
        non_existing ->
            ?FAIL_MSG("~s requires floppystore to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

    Nodes   = basho_bench_config:get(floppystore_nodes),
    Cookie  = basho_bench_config:get(floppystore_cookie),
    MyNode  = basho_bench_config:get(floppystore_mynode, [basho_bench, longnames]),

    %% Try to spin up net_kernel
    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    %% Initialize cookie for each of the nodes
    [true = erlang:set_cookie(N, Cookie) || N <- Nodes],
    %true = erlang:set_cookie(Node, Cookie),

    %% Try to ping each of the nodes
    ping_each(Nodes),

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(Nodes)+1), Nodes),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),

    {ok, #state{node=TargetNode}}.




run(read, KeyGen, _ValueGen, State=#state{node=Node}) ->
    %State1 = maybe_sync(State),
    Key = KeyGen(),
    Res = rpc:call(Node, floppy, read, [Key, riak_dt_gcounter], ?TIMEOUT),
    case Res of
        {ok, _Value} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {badrpc, Reason};
        _Reason ->
            {ok, State}
    end;
run(append, KeyGen, ValueGen, State=#state{node=Node}) ->
    Key = KeyGen(),
    Param = ValueGen(),
    Res = rpc:call(Node, floppy, append, [Key, Param], ?TIMEOUT),
    %State1 = maybe_sync(State),
    case Res of
        {ok, _Result} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {badrpc, Reason};
        _Reason ->
            {ok, State}
    end.


floppy_valgen(Id) ->
    fun() ->
        {increment, Id}
    end.

ping_each([]) ->
    ok;
ping_each([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong ->
            ?INFO("Finished pinging ~p", [Node]),
            ping_each(Rest);
        pang ->
            ?FAIL_MSG("Failed to ping node ~p\n", [Node])
    end.
