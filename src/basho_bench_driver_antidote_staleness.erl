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
-module(basho_bench_driver_antidote_staleness).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 20000).
-record(state, {node,
                worker_id,
                time,
                type_dict}).

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
    Types  = basho_bench_config:get(antidote_types),

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
    AvailableNodes = ping_each(Nodes, []),

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(AvailableNodes)+1), AvailableNodes),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    %KeyDict= dict:new(),
    TypeDict = dict:from_list(Types),
    {ok, #state{node=TargetNode, time={1,1,1}, worker_id=Id, type_dict=TypeDict}}.

%% @doc Read a key
run(read, KeyGen, _ValueGen, State=#state{node=Node, type_dict=TypeDict}) ->
    Key = KeyGen(),
    Type = get_key_type(Key, TypeDict),
    Response = rpc:call(Node, antidote, read, [Key, Type]),
    case Response of
        {ok, _Value} ->
            report_staleness(Node),
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Write to a key
run(append, KeyGen, ValueGen,
    State=#state{node=Node, worker_id=Id, type_dict=TypeDict}) ->
    Key = KeyGen(),
    Type = get_key_type(Key, TypeDict),
    {Type, KeyParam} = get_random_param(TypeDict, Type, Id, ValueGen()),
    Response = rpc:call(Node, antidote, append, [Key, Type, KeyParam]),
    case Response of
        {ok, _Result} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.

%% Private
ping_each([], Acc) ->
    Acc;
ping_each([Node | Rest], Acc) ->
    case net_adm:ping(Node) of
        pong ->
            ?INFO("Finished pinging ~p", [Node]),
            ping_each(Rest, Acc ++ [Node]);
        pang ->
            ?INFO("Failed to ping node ~p\n", [Node]),
            ping_each(Rest, Acc)
    end.

get_key_type(Key, Dict) ->
    Keys = dict:fetch_keys(Dict),
    RanNum = Key rem length(Keys),
    lists:nth(RanNum+1, Keys).

get_random_param(Dict, Type, Actor, Value) ->
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of
        riak_dt_gcounter ->
           {riak_dt_gcounter, {lists:nth(Num, Params), Actor}};
        crdt_pncounter ->
           {crdt_pncounter, {lists:nth(Num, Params), Actor}};
        riak_dt_gset ->
           {riak_dt_gset, {{lists:nth(Num, Params), Value}, Actor}};
        crdt_orset ->
           {crdt_orset, {{lists:nth(Num, Params), Value}, Actor}}
        %crdt_pncounter ->
        %   {crdt_pncounter, {lists:nth(Num, Params), Actor}};
        %crdt_orset ->
        %   {crdt_orset, {{lists:nth(Num, Params), Value}, Actor}}
    end.

report_staleness(Node) ->
    CurTime = now_microsec(),
    {ok, SS} = rpc:call(Node, vectorclock, get_stable_snapshot, []),
    %% Here it is assumed the stable snapshot has entries for all remote DCs
    SSL = lists:keysort(1, dict:to_list(SS)),
    Staleness = lists:map(fun({_Dc, Time}) ->
                                  max(0, CurTime - Time)
                          end, SSL),
    report_staleness_rec(Staleness, 1).

report_staleness_rec([],_) -> ok;
report_staleness_rec([H|T], Iter) ->
    Op=list_to_atom(string:concat(atom_to_list(staleness), integer_to_list(Iter))),
    folsom_metrics:notify({latencies, {Op, Op}}, H),
    folsom_metrics:notify({units, {Op, Op}}, {inc, 1}),
    report_staleness_rec(T, Iter+1).
    

now_microsec() ->    
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.                                                  
