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
-record(state, {node,
                type_dict,
                key_dict}).

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
    Types  = basho_bench_config:get(floppystore_types),

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
    KeyDict= dict:new(),
    TypeDict = dict:from_list(Types),
    {ok, #state{node=TargetNode, type_dict=TypeDict, key_dict=KeyDict}}.


run(read, KeyGen, _ValueGen, State=#state{node=Node, key_dict=KeyDict}) ->
    %State1 = maybe_sync(State),
    Key = KeyGen(),
    KeyType = case dict:find(Key, KeyDict) of
                {ok, Type} -> io:format("ReadType:~w~n", [Type]), hd(Type);
                 _ ->  riak_dt_gcounter 
              end,
    Res = rpc:call(Node, floppy, read, [Key, KeyType], ?TIMEOUT),
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
run(append, KeyGen, ValueGen, State=#state{node=Node, key_dict=KeyDict, type_dict=TypeDict}) ->
    Key = KeyGen(),
    {NewKeyDict, KeyParam} = case dict:find(Key, KeyDict) of
                            {ok, Type} -> 
                                Param = get_random_param(TypeDict, hd(Type), ValueGen),
                                {KeyDict, Param};
                            error ->   
                                Types = dict:fetch_keys(TypeDict), 
                                Type = get_random_elem(Types),
                                Dict2 = dict:append(Key, Type, KeyDict),
                                Param = get_random_param(TypeDict, Type, ValueGen),
                                {Dict2, Param}
              end,
    Res = rpc:call(Node, floppy, append, [Key, KeyParam], ?TIMEOUT),
    case Res of
        {ok, _Result} ->
            {ok, State#state{key_dict=NewKeyDict}};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {badrpc, Reason};
        _ ->
            {ok, State}
    end.


floppy_valgen(Id) ->
    Id.

%% Private
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

get_random_elem(List) ->
    random:seed(now()),
    Num = random:uniform(length(List)),
    lists:nth(Num, List).

get_random_param(Dict, Type, Actor) -> 
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of 
        riak_dt_gcounter ->
            {lists:nth(Num, Params), Actor};
        riak_dt_gset ->
            {{lists:nth(Num, Params), random:uniform(10000)}, Actor}
    end.

