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
-module(basho_bench_driver_antidote_simple).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 20000).
-record(state, {worker_id,
                time,
                num_updates,
                num_reads,
                clock,
                pb_port,
                target_node}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure bitcask is available
    case code:which(antidote) of
        non_existing ->
            ?FAIL_MSG("~s requires antidote to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

    Nodes   = basho_bench_config:get(antidote_nodes),
    Cookie  = basho_bench_config:get(antidote_cookie),
    MyNode  = basho_bench_config:get(antidote_mynode, [basho_bench, longnames]),
    NumReads  = basho_bench_config:get(num_reads),
    NumUpdates  = basho_bench_config:get(num_updates),

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
    true = erlang:set_cookie(node(), Cookie),
    [true = erlang:set_cookie(N, Cookie) || N <- Nodes],

    %% Try to ping each of the nodes

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(Nodes)+1), Nodes),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    %KeyDict= dict:new(),
    {ok, #state{target_node=TargetNode, worker_id=Id, 
               num_updates = NumUpdates, num_reads=NumReads}}.

%% @doc Read a key
run(read_txn, _KeyGen, _ValueGen, State=#state{worker_id = Id, target_node=Node, num_reads=NumReads, clock=Clock}) ->
    Keys = k_unique_numes(NumReads, 1000),
    ReadKeys = [{read, {K, riak_dt_lwwreg}}   || K <- Keys],
    Response = rpc:call(Node, antidote, read_objects, [Clock, ignore, ReadKeys]), %antidotec_pb_socket:get_crdt(Key, Type, Pid),
    case Response of
        {ok, _Value, CommitTime} ->
            {ok, State#state{clock=CommitTime}};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            {error, timeout, State};            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(update_txn, _KeyGen, _ValueGen, State=#state{worker_id = Id, target_node=Node, num_updates=NumUpdates,
        clock=Clock}) ->
    Keys = k_unique_numes(NumUpdates, 1000),
    %lager:info("Keys are ~w", [Keys]),
    Updates = [{update, {Key, riak_dt_lwwreg, {{assign, random_string(10)}, haha}}} || Key <- Keys],
    %lager:info("Updates are ~w", [Updates]),
    %lager:info("Deps are ~w", [Deps]),
    Response = rpc:call(Node, antidote, update_objects, [Clock, ignore, Updates]),
    %lager:info("Response is ~w", [Response]),
    case Response of
        {ok, TS} ->
            {ok, State#state{clock=TS}};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            {error, timeout, State};            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        error ->
            {error, abort, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.


random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

k_unique_numes(Num, Range) ->
    Seq = lists:seq(1, Num),
    S = lists:foldl(fun(_, Set) ->
                N = uninum(Range, Set),
                 sets:add_element(N, Set)
                end, sets:new(), Seq),
    sets:to_list(S).

uninum(Range, Set) ->
    R = random:uniform(Range),
    case sets:is_element(R, Set) of
        true ->
            uninum(Range, Set);
        false ->
            R
    end.

