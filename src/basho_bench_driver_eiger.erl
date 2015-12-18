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
-module(basho_bench_driver_eiger).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 20000).
-record(state, {worker_id,
                time,
                type_dict,
                pb_pid,
		        num_partitions,
		        set_size,
                deps :: dict(),
                num_reads :: non_neg_integer(),
                num_updates :: non_neg_integer(),
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

    IPs = basho_bench_config:get(antidote_pb_ips),
    PbPorts = basho_bench_config:get(antidote_pb_port),
    Types  = basho_bench_config:get(antidote_types),
    SetSize = basho_bench_config:get(set_size),
    NumReads  = basho_bench_config:get(num_reads),
    NumUpdates = basho_bench_config:get(num_updates),
    NumPartitions = length(IPs),

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    %%TargetPort = lists:nth((Id rem length(PbPorts)+1), PbPorts),
    TargetPort = PbPorts,
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),

    {ok, Pid} = antidotec_pb_socket:start_link(TargetNode, TargetPort),
    TypeDict = dict:from_list(Types),
    {ok, #state{time={1,1,1}, worker_id=Id,
		pb_pid = Pid,
		set_size = SetSize,
        deps=dict:new(),
		num_partitions = NumPartitions,
		type_dict = TypeDict, pb_port=TargetPort,
        num_reads = NumReads, num_updates= NumUpdates,
		target_node=TargetNode}}.

%% @doc Read a key
run(read, KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=_Port, target_node=_Node, deps=OldDeps}) ->
    KeyInt = KeyGen(),
    Key = list_to_binary(integer_to_list(KeyInt)),

    Bound_object = {Key, riak_dt_lwwreg, <<"bucket">>},
    case antidotec_pb:start_transaction(Pid, term_to_binary(ignore), [{static, true}]) of
	{ok, TxId} ->
	    case antidotec_pb:read_objects(Pid, [Bound_object], TxId) of
		{ok, [_Val]} ->		    
		    case antidotec_pb:commit_transaction(Pid, TxId) of
			{ok, BDeps} ->
                Deps = binary_to_term(BDeps),
                %lager:info("Deps are ~w", [Deps]),
			    {ok, State#state{deps=merge_deps(Deps, OldDeps)}};
			_ ->
			    lager:info("Error read1 on client ~p",[Id]),
			    {error, timeout, State}
		    end;
		Error ->
		    lager:info("Error read2 on client ~p : ~p",[Id, Error]),
		    {error, timeout, State}
	    end;
	_ ->
	    lager:info("Error read3 on client ~p",[Id]),
	    {error, timeout, State}
    end;

%% @doc Read a key
run(read_txn, _KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=_Port, target_node=_Node, num_reads=NumReads,
        deps=OldDeps}) ->
    IntKeys = k_unique_numes(NumReads, 1000),
    BoundObjects = [{list_to_binary(integer_to_list(K)), riak_dt_lwwreg, <<"bucket">>} || K <- IntKeys ],

    case antidotec_pb:start_transaction(Pid, term_to_binary(ignore), [{static, true}]) of
    {ok, TxId} ->
        case antidotec_pb:read_objects(Pid, BoundObjects, TxId) of
        {ok, _} ->
            case antidotec_pb:commit_transaction(Pid, TxId) of
            {ok, BDeps} ->
                Deps = binary_to_term(BDeps),
                %lager:info("Deps are ~w", [Deps]),
                {ok, State#state{deps=merge_deps(Deps, OldDeps)}};
            _ ->
                lager:info("Error read1 on client ~p",[Id]),
                {error, timeout, State}
            end;
        Error ->
            lager:info("Error read2 on client ~p : ~p",[Id, Error]),
            {error, timeout, State}
        end;
    _ ->
        lager:info("Error read3 on client ~p",[Id]),
        {error, timeout, State}
    end;

%% Response =  antidotec_pb_socket:get_crdt(Key, Type, Pid),
    %% case Response of
    %%     {ok, _Value} ->
    %%         {ok, State};
    %%     {error,timeout} ->
    %%         lager:info("Timeout on client ~p",[Id]),
    %%         antidotec_pb_socket:stop(Pid),
    %%         {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
    %%         {error, timeout, State#state{pb_pid=NewPid}    };            
    %%     {error, Reason} ->
    %%         lager:error("Error: ~p",[Reason]),
    %%         {error, Reason, State};
    %%     {badrpc, Reason} ->
    %%         {error, Reason, State}
    %% end;


%% @doc Multikey txn 
run(read_all_write_one, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id, num_partitions=NumPart, pb_port=_Port, target_node=_Node, type_dict=TypeDict}) ->
    KeyInt = KeyGen(),
    KeyList = lists:seq(KeyInt, KeyInt+NumPart-1), 
    KeyTypeList = get_list_key_type(KeyList, TypeDict, []),
    Bucket = <<"bucket">>,
    ObjectList = lists:map( fun({Key, Type}) ->
                                    {Key, Type, Bucket}
                            end,
                            KeyTypeList
                          ),
    %% Snapshot read a list of objects
    case antidotec_pb:start_transaction(Pid, term_to_binary(ignore), [{static, true}]) of
	{ok, TxId} ->
	    case antidotec_pb:read_objects(Pid, ObjectList, TxId) of
		{ok, [_Val]} ->		    
		    case antidotec_pb:commit_transaction(Pid, TxId) of
			{ok, _} ->
                            %% append one object
                            run(append, KeyGen, ValueGen, State);
			_ ->
			    lager:info("Error read1 on client ~p",[Id]),
			    {error, timeout, State}
		    end;
		Error ->
		    lager:info("Error read2 on client ~p : ~p",[Id, Error]),
		    {error, timeout, State}
	    end;
	_ ->
	    lager:info("Error read3 on client ~p",[Id]),
	    {error, timeout, State}
    end;
    

run(append_multiple, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node, type_dict=TypeDict}) ->
    KeyInt = KeyGen(),
    KeyList = lists:seq(KeyInt, KeyInt+8), 
    Value = ValueGen(),
    OpList = lists:foldl(fun(X, Acc) -> 
                                Type = get_key_type(X, TypeDict),
                                {Mod, Op, Param} = get_random_param(TypeDict, Type, Value),
                                Acc++[Mod:Op(Param, Mod:new(list_to_binary(integer_to_list(X))))] 
                         end, [], KeyList),
    Response =  antidotec_pb_socket:atomic_store_crdts(OpList, Pid),
    case Response of
        {ok, _} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}    };            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        error ->
            lager:info("Error!!!"),
            {error, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Write to a key
run(append, KeyGen, _ValueGen,
    State=#state{type_dict=_TypeDict,
                 pb_pid = Pid,
                 worker_id = Id,
                 pb_port=_Port,
                 deps=Deps,
                 target_node=_Node}) ->
    KeyInt = KeyGen(),
    Key = list_to_binary(integer_to_list(KeyInt)),
    
    BObj = {{Key, riak_dt_lwwreg, <<"bucket">>}, assign, random_string(10)},
    
    case antidotec_pb:start_transaction(Pid, term_to_binary(dict:to_list(Deps)), [{static, true}]) of
	{ok, TxId} ->
	    case antidotec_pb:update_objects(Pid, [BObj], TxId) of
		ok ->
		    case antidotec_pb:commit_transaction(Pid, TxId) of
			{ok, BTS} ->
                TS = binary_to_term(BTS),
                NewDeps = [{Key, TS}], 
                %lager:info("NewDeps are ~w, updated ~w, put ~w", [NewDeps, Key, Deps]),
			    {ok, State#state{deps=dict:from_list(NewDeps)}};
			Error ->
			    {error, Error, State}
		    end;
		Error ->
		    lager:info("Error append2 on client ~p : ~p",[Id, Error]),
                    {error, Error, State}
	    end;
        Error ->
	    lager:info("Error append3 on client ~p",[Id]),
	    {error, Error, State}
    end;

%% @doc Write to a key
run(append_txn, _KeyGen, _ValueGen,
    State=#state{type_dict=_TypeDict,
                 pb_pid = Pid,
                 worker_id = Id,
                 pb_port=_Port,
                 deps=Deps,
                 num_updates=NumUpdates,
                 target_node=_Node}) ->
    IntKeys = k_unique_numes(NumUpdates, 1000),
    BKeys = [list_to_binary(integer_to_list(K)) || K <- IntKeys],
    BObjs = [{{K, riak_dt_lwwreg, <<"bucket">>}, 
            assign, random_string(10)} || K <- BKeys ],

    case antidotec_pb:start_transaction(Pid, term_to_binary(dict:to_list(Deps)), [{static, true}]) of
    {ok, TxId} ->
        case antidotec_pb:update_objects(Pid, BObjs, TxId) of
        ok ->
            case antidotec_pb:commit_transaction(Pid, TxId) of
            {ok, BTS} ->
                TS = binary_to_term(BTS),
                NewDeps = [{Key, TS} || Key <- BKeys ],
                %lager:info("NewDeps are ~w, updated ~p, put ~w", [NewDeps, IntKeys, Deps]),
                {ok, State#state{deps=dict:from_list(NewDeps)}};
            Error ->
                {error, Error, State}
            end;
        Error ->
            lager:info("Error append2 on client ~p : ~p",[Id, Error]),
                    {error, Error, State}
        end;
        Error ->
        lager:info("Error append3 on client ~p",[Id]),
        {error, Error, State}
    end;


run(update, KeyGen, ValueGen,
    State=#state{type_dict=TypeDict,
                 pb_pid = Pid,
                 worker_id = Id,
                 pb_port=Port,
		 set_size=SetSize,
                 target_node=Node}) ->
    KeyInt = KeyGen(),
    Key = list_to_binary(integer_to_list(KeyInt)),
    %%TODO: Support for different data types
    Type = get_key_type(KeyInt, TypeDict),
    Response =  case antidotec_pb_socket:get_crdt(Key, Type, Pid) of
                    {ok, CRDT} ->
                        {Mod, Op, Param} = get_random_param(TypeDict, Type, ValueGen(), CRDT, SetSize),
                        Obj = Mod:Op(Param,CRDT),
                        antidotec_pb_socket:store_crdt(Obj, Pid);
                    Other -> Other
                end,
    case Response of
        ok ->
            {ok, State};
        {ok, _Result} ->
            {ok, State};
        {error,timeout}->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}}; 
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.


get_list_key_type([], _Dict, Acc) ->
    Acc;
get_list_key_type([Key|Rest], Dict, Acc) ->
    Pair = {list_to_binary(integer_to_list(Key)), get_key_type(Key, Dict)},
    get_list_key_type(Rest, Dict, [Pair | Acc]).

get_key_type(Key, Dict) ->
    Keys = dict:fetch_keys(Dict),
    RanNum = Key rem length(Keys),
    lists:nth(RanNum+1, Keys).

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

get_random_param(Dict, Type, Value, Obj, SetSize) ->
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of
        riak_dt_pncounter ->
           {antidotec_counter,lists:nth(Num, Params), 1};
        riak_dt_orset ->
            Set = antidotec_set:value(Obj),
            %%Op = lists:nth(Num, Params),
	    NewOp = case sets:size(Set) =< SetSize of
                true ->
                    add;
                false ->
                    remove
                end,
            case NewOp of 
                remove ->
                    case sets:to_list(Set) of 
                        [] -> {antidotec_set, add, Value};                     
                        [H|_T] -> {antidotec_set, remove, H}
                    end;
                _ ->
                    {antidotec_set, NewOp, Value}
            end                      
    end.

merge_deps(NewDeps, OldDeps) ->
    lists:foldl(fun({Key, TS}, Dict) ->
                case TS of 
                    {ignore, 0} ->
                        Dict;
                    _ ->
                        case dict:find(Key, Dict) of
                            error ->
                                dict:store(Key, TS, Dict);
                            {ok, TS} ->
                                Dict;
                            _ ->
                                dict:store(Key, TS, Dict)
                        end
                end end, OldDeps, NewDeps).

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

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

