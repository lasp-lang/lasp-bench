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
-module(basho_bench_driver_antidote_pb).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 20000).
-record(state, {
                worker_id,
                time,
                type_dict,
                pb_pid}).

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

    IPs = basho_bench_config:get(antidote_pb_ips),
    PbPort = basho_bench_config:get(antidote_pb_port),
    Types  = basho_bench_config:get(antidote_types),

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),

    {ok, Pid} = antidotec_pb_socket:start_link(TargetNode, PbPort),
    TypeDict = dict:from_list(Types),
    {ok, #state{time={1,1,1}, worker_id=Id,
               pb_pid = Pid,
               type_dict = TypeDict}}.

%% @doc Read a key
run(read, KeyGen, _ValueGen, State=#state{pb_pid = Pid}) ->
    KeyInt = KeyGen(),
    Key = list_to_binary(integer_to_list(KeyInt)),
    Type = get_key_type(KeyInt),
    Response =  antidotec_pb_socket:get_crdt(Key, Type, Pid),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Write to a key
run(append, KeyGen, ValueGen,
    State=#state{type_dict=TypeDict,
                 pb_pid = Pid}) ->
    KeyInt = KeyGen(),
    Key = list_to_binary(integer_to_list(KeyInt)),
    %%TODO: Support for different data types
    Type = get_key_type(KeyInt),
    {Mod, Op, Param} = get_random_param(TypeDict, Type, ValueGen()),
    Obj = Mod:Op(Param, Mod:new(Key)),
    Response = antidotec_pb_socket:store_crdt(Obj, Pid),
    case Response of
        ok ->
            {ok, State};
        {ok, _Result} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.

get_key_type(Key) ->
    case (Key rem 10) > 5 of
        true ->
            riak_dt_pncounter;
        false ->
            riak_dt_orset
    end.

get_random_param(Dict, Type, Value) ->
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of
        riak_dt_pncounter ->
           {antidotec_counter,lists:nth(Num, Params), 1};
        riak_dt_orset ->
           {antidotec_set, lists:nth(Num, Params), Value}
    end.
