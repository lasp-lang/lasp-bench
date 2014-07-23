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
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 1000).
-record(state, {node,
                worker_id,
                time,
                type_dict}).
                %key_dict}).

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
    %KeyDict= dict:new(),
    TypeDict = dict:from_list(Types),
    {ok, #state{node=TargetNode, time={1,1,1}, worker_id=Id, type_dict=TypeDict}}.

%% @doc Read a key
run(read, KeyGen, _ValueGen, State=#state{node=Node}) ->
    Key = KeyGen(),
    KeyType = get_key_type(Key), 
    BinaryKey = Key,%<<(Key):32/big>>,
    Res = rpc:call(Node, floppy, read, [BinaryKey, KeyType], ?TIMEOUT),
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
%% @doc Write to a key
run(append, KeyGen, ValueGen, State=#state{node=Node, worker_id=Id, type_dict=TypeDict}) ->
    Key = KeyGen(),
    Type = get_key_type(Key),
    BinaryKey = Key,%<<(Key):32/big>>,
    KeyParam = get_random_param(TypeDict, Type, Id, ValueGen()),
    Res = rpc:call(Node, floppy, append, [BinaryKey, KeyParam], ?TIMEOUT),
    case Res of
        {ok, _Result} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {badrpc, Reason};
        _ ->
            {ok, State}
    end;
%% @doc Start a static transaction
run(static_tx, KeyGen, ValueGen, State=#state{node=Node, time=ClientTime, worker_id=Id, type_dict=Dict}) ->
    random:seed(now()),
    NumAppend = random:uniform(10),
    NumRead = random:uniform(10),
    ListAppends = get_random_append_ops(NumAppend, Dict, Id, KeyGen, ValueGen),
    ListReads = get_random_read_ops(NumRead, KeyGen),
    ListOps = ListAppends++ListReads,
    Res = rpc:call(Node, floppy, clockSI_execute_TX, [ClientTime, ListOps], ?TIMEOUT),
    case Res of
        {ok, _ReadSet, CommitTime} ->
            {ok, State#state{time=CommitTime}};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {badrpc, Reason};
        _Reason ->
            {ok, State}
    end;
run(interactive_tx, KeyGen, ValueGen, State=#state{node=Node, worker_id=Id, type_dict=Dict}) ->
    random:seed(now()),
    NumAppend = random:uniform(10),
    NumRead = random:uniform(10),
    ListAppends = get_random_append_ops(NumAppend, Dict, Id, KeyGen, ValueGen),
    ListReads = get_random_read_ops(NumRead, KeyGen),
    ListOps = ListAppends++ListReads,
    RandomOps = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- ListOps])],
    {ok, TxId} = rpc:call(Node, floppy, clockSI_istart_tx, [now()], ?TIMEOUT),
    ExecuteFun = fun(X) ->  case X of 
                            {update, UKey, UParam} -> ok = rpc:call(Node, floppy, clockSI_iupdate, [TxId, UKey, UParam]);
                            {read, RKey, RType} -> {ok, _} = rpc:call(Node, floppy, clockSI_iread, [TxId, RKey, RType])
                            end
                 end,
    lists:foreach(ExecuteFun, RandomOps),  
    {ok, _} = rpc:call(Node, floppy, clockSI_iprepare, [TxId]),
    End=rpc:call(Node, floppy, clockSI_icommit, [TxId]),
    case End of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {badrpc, Reason};
        _Reason ->
            {ok, State}
    end.



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

get_key_type(_Key) ->
    %Rem = Key rem 10,
    %case Rem > 5 of
    %    true ->
            riak_dt_gcounter.
    %    false ->
    %        riak_dt_gset
    %end.

get_random_param(Dict, Type, Actor, Value) -> 
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of 
        riak_dt_gcounter ->
            {lists:nth(Num, Params), Actor};
        riak_dt_gset ->
            {{lists:nth(Num, Params), Value}, Actor}
    end.


get_random_append_op(Key, Dict, Actor, Value) ->
    Type = get_key_type(Key),
    Params = get_random_param(Dict, Type, Actor, Value),
    {update, Key, Params}.

get_random_read_op(Key) ->
    Type = get_key_type(Key),
    {read, Key, Type}.
    
get_random_append_ops(Num, Dict, Actor, KeyGen, ValueGen) ->
    %KeyList = [<<(KeyGen()):32/big>> || _ <- lists:seq(1, Num)],
    KeyList = [KeyGen() || _ <- lists:seq(1, Num)],
    [get_random_append_op(Key, Dict, Actor, ValueGen()) || Key <- KeyList].

get_random_read_ops(Num, KeyGen) ->
    %KeyList = [<<(KeyGen()):32/big>> || _ <- lists:seq(1, Num)],
    KeyList = [KeyGen() || _ <- lists:seq(1, Num)],
    [get_random_read_op(Key) || Key <- KeyList].
