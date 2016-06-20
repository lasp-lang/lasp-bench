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
-record(state, {worker_id,
                time,
                type_dict,
		last_read,
                pb_pid,
		        num_partitions,
		        set_size,
                commit_time,
                num_reads,
                num_updates,
                pb_port,
                target_node,
                measure_staleness,
                temp_num_reads,
                temp_num_updates,
    sequential_reads,
    sequential_writes}).

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

    random:seed(now()),

    IPs = basho_bench_config:get(antidote_pb_ips),
    PbPort = basho_bench_config:get(antidote_pb_port),
    Types  = basho_bench_config:get(antidote_types),
    SetSize = basho_bench_config:get(set_size),
    NumUpdates  = basho_bench_config:get(num_updates),
    NumReads = basho_bench_config:get(num_reads),
    NumPartitions = basho_bench_config:get(num_vnodes),
    MeasureStaleness = basho_bench_config:get(staleness),
    SequentialReads = basho_bench_config:get(sequential_reads),
    SequentialWrites = basho_bench_config:get(sequential_writes),


    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),

    {ok, Pid} = antidotec_pb_socket:start_link(TargetNode, PbPort),
    TypeDict = dict:from_list(Types),
    {ok, #state{time={1,1,1}, worker_id=Id,
		pb_pid = Pid,
		last_read={undefined,undefined},
		set_size = SetSize,
		num_partitions = NumPartitions,
		type_dict = TypeDict, pb_port=PbPort,
		target_node=TargetNode, commit_time=ignore,
        num_reads=NumReads, num_updates=NumUpdates,
        temp_num_reads=NumReads, temp_num_updates=NumUpdates,
        measure_staleness=MeasureStaleness,
        sequential_reads = SequentialReads,
        sequential_writes = SequentialWrites}}.

%% A more general static transaction.
%% combienes the previous read_txn and update_txn
%% Reads from a number of keys defined in the config file: num_reads, then
%% updates a number of keys defined in the config file: num_updates.


%% READ ONLY TXN!!!
run(txn, KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id,
    pb_port=_Port, target_node=_Node,
    num_reads=NumReads,
    num_updates = 0,
    type_dict = TypeDict,
    set_size=SetSize,
    commit_time=OldCommitTime,
    sequential_reads = SeqReads}) ->
    IntKeys = generate_keys(NumReads, KeyGen),
    BoundObjects = [{list_to_binary(integer_to_list(K)), get_key_type(K, TypeDict), <<"bucket">>} || K <- IntKeys ],
%  BoundObjects = [{list_to_binary(integer_to_list(K)), riak_dt_lwwreg, <<"bucket">>} || K <- IntKeys ],
    case antidotec_pb:start_transaction(Pid, term_to_binary(OldCommitTime), [{static, false}]) of
        {ok, TxId} ->
            case create_read_operations(Pid, BoundObjects, TxId, SeqReads) of
                {ok, _ReadResult} ->
                            case antidotec_pb:commit_transaction(Pid, TxId) of
                                {ok, BCommitTime} ->
                                    CommitTime = binary_to_term(BCommitTime),
                                    {ok, State#state{commit_time=CommitTime}};
                                Error ->
                                    {error, {Id, Error}, State}
                            end;
                Error ->
                    {error, {Id, Error}, State}
            end;
        Error ->
            {error, {Id, Error}, State}
    end;


%% WRITE TXN!!!
run(txn, KeyGen, ValueGen, State = #state{pb_pid = Pid, worker_id = Id,
    pb_port = _Port, target_node = _Node,
    num_reads = 0,
    num_updates = NumUpdates,
    type_dict = TypeDict,
    set_size = SetSize,
    commit_time = OldCommitTime,
    sequential_writes = SeqWrites}) ->
    case antidotec_pb:start_transaction(Pid, term_to_binary(OldCommitTime), [{static, false}]) of
        {ok, TxId} ->
            UpdateIntKeys = generate_keys(NumUpdates, KeyGen),
            BObjs = multi_get_random_param_new(UpdateIntKeys, TypeDict, ValueGen(), undefined, SetSize),
            case create_update_operations(Pid, BObjs, TxId, SeqWrites) of
                ok ->
                    case antidotec_pb:commit_transaction(Pid, TxId) of
                        {ok, BCommitTime} ->
                            CommitTime = binary_to_term(BCommitTime),
                            {ok, State#state{commit_time = CommitTime}};
                        Error ->
                            {error, {Id, Error}, State}
                    end;
                Error ->
                    {error, {Id, Error}, State}
            end;        Error ->
        {error, {Id, Error}, State}
    end;




run(txn, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id,
    pb_port=_Port, target_node=_Node,
    num_reads=NumReads,
    num_updates = NumUpdates,
    type_dict = TypeDict,
    set_size=SetSize,
    commit_time=OldCommitTime,
    sequential_writes = SeqWrites,
    sequential_reads = SeqReads}) ->
%%    io:format("~nNumReads = ~w  NumUpdates = ~w",[NumReads, NumUpdates]),
    IntKeys = generate_keys(NumReads, KeyGen),
    BoundObjects = [{list_to_binary(integer_to_list(K)), get_key_type(K, TypeDict), <<"bucket">>} || K <- IntKeys ],
%  BoundObjects = [{list_to_binary(integer_to_list(K)), riak_dt_lwwreg, <<"bucket">>} || K <- IntKeys ],
    case antidotec_pb:start_transaction(Pid, term_to_binary(OldCommitTime), [{static, false}]) of
        {ok, TxId} ->
            case create_read_operations(Pid, BoundObjects, TxId, SeqReads) of
                {ok, _ReadResult} ->
%%                    UpdateIntKeys = generate_keys(NumUpdates, KeyGen),
%%                    The following selects the latest reads for updating.
                    UpdateIntKeys = lists:sublist(IntKeys, NumReads - NumUpdates +1, NumUpdates),
                    %    BoundObjects = [{list_to_binary(integer_to_list(K)), get_key_type(K, TypeDict), <<"bucket">>} || K <- IntKeys ],
                    %  BKeys = [list_to_binary(integer_to_list(K1)) || K1 <- UpdateIntKeys],
                    BObjs = multi_get_random_param_new(UpdateIntKeys, TypeDict, ValueGen(), undefined, SetSize),
                    %BObjs = [{{K1, riak_dt_lwwreg, <<"bucket">>},
                    %  assign, random_string(10)} || K1 <- BKeys ],

%%            lager:info("Sending this updates ~p",[BObjs]),
                    case create_update_operations(Pid, BObjs, TxId, SeqWrites) of
                        ok ->
                            case antidotec_pb:commit_transaction(Pid, TxId) of
                                {ok, BCommitTime} ->
                                    CommitTime =
%%                          case BCommitTime of
%%                                       ignore ->
%%                                           ignore;
%%                                       _->
                                    binary_to_term(BCommitTime),
%%                                   end,
%%                        lager:info("BCommitTime ~p",[BCommitTime]),

                                    {ok, State#state{commit_time=CommitTime}};
                                Error ->
                                    {error, {Id, Error}, State}
                            end;
                        Error ->
                            {error, {Id, Error}, State}
                    end;
                Error ->
                    {error, {Id, Error}, State}
            end;
        Error ->
            {error, {Id, Error}, State}
    end;







%% A  static transaction that reads and updates the same objects.
%% Reads from a number of keys defined in the config file: num_reads, then
%% updates the same keys (the num_updates in the config is unused).

%%run(append, KeyGen, ValueGen, State) ->
%%    %% this reads first, and then updates.
%%    run(txn, KeyGen, ValueGen, State#state{num_reads = State#state.temp_num_updates,num_updates=State#state.temp_num_updates});
%%run(read, KeyGen, ValueGen, State) ->
%%    run(txn, KeyGen, ValueGen, State#state{num_updates = 0,num_reads=State#state.temp_num_reads});
run(append, KeyGen, ValueGen, State) ->
    run(txn, KeyGen, ValueGen, State);
run(read, KeyGen, ValueGen, State) ->
    run(txn, KeyGen, ValueGen, State);

run(rw_txn, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id,
  pb_port=_Port, target_node=_Node,
  num_reads=NumReads,
  type_dict = TypeDict,
  set_size=SetSize,
  commit_time=OldCommitTime,
    sequential_reads = SeqReads,
    sequential_writes = SeqWrites}) ->

  IntKeys = generate_keys(NumReads, KeyGen),
  BoundObjects = [{list_to_binary(integer_to_list(K)), get_key_type(K, TypeDict), <<"bucket">>} || K <- IntKeys ],
%  BoundObjects = [{list_to_binary(integer_to_list(K)), riak_dt_lwwreg, <<"bucket">>} || K <- IntKeys ],
  case antidotec_pb:start_transaction(Pid, term_to_binary(OldCommitTime), [{static, false}]) of
    {ok, TxId} ->
        case create_read_operations(Pid, BoundObjects, TxId, SeqReads) of
            {ok, Values} ->
                BObjs = multi_get_random_param_new(IntKeys, TypeDict, ValueGen(), Values, SetSize),
                %BObjs = [{{K1, riak_dt_lwwreg, <<"bucket">>},
                %  assign, random_string(10)} || K1 <- BKeys ],
                case create_update_operations(Pid, BObjs, TxId, SeqWrites) of
                    ok ->
                        case antidotec_pb:commit_transaction(Pid, TxId) of
                            {ok, BCommitTime} ->
                                CommitTime = binary_to_term(BCommitTime),
                                {ok, State#state{commit_time = CommitTime}};
                            Error ->
                                {error, {Id, Error}, State}
                        end;
                    Error ->
                        {error, {Id, Error}, State}
                end;
            Error ->
                {error, {Id, Error}, State}
        end;
      Error ->
          {error, {Id, Error}, State}
  end.



create_read_operations(Pid, BoundObjects, TxId, IsSeq) ->
    case IsSeq of
        true->
            Result = lists:map(fun(BoundObj)->
                {ok, [Value]} = antidotec_pb:read_objects(Pid, [BoundObj], TxId),
                        Value
                end,BoundObjects),
            {ok, Result};
        false ->
                antidotec_pb:read_objects(Pid, BoundObjects, TxId)
    end.

create_update_operations(Pid, BoundObjects, TxId, IsSeq) ->
    case IsSeq of
        true ->
            lists:map(fun(BoundObj) ->
                antidotec_pb:update_objects(Pid, [BoundObj], TxId)
                               end, BoundObjects),
            ok;
        false ->
            antidotec_pb:update_objects(Pid, BoundObjects, TxId)
    end.



        get_key_type(Key, Dict) ->
    Keys = dict:fetch_keys(Dict),
    RanNum = Key rem length(Keys),
    lists:nth(RanNum+1, Keys).


multi_get_random_param_new(KeyList, Dict, Value, Objects, SetSize) ->
  multi_get_random_param_new(KeyList, Dict, Value, Objects, SetSize, []).

multi_get_random_param_new([], _Dict, _Value, _Objects, _SetSize, Acc)->
 % lager:info("Acumulatore: ~p", [Acc]),
  Acc;
multi_get_random_param_new([Key|Rest], Dict, Value, Objects, SetSize, Acc)->
  Type = get_key_type(Key, Dict),
  case Objects of
    undefined ->
      Obj = undefined,
      ObjRest = undefined;
    [H|T]->
      Obj = H,
      ObjRest = T
  end,
  [Param] = get_random_param_new(Key, Dict, Type, Value, Obj, SetSize),
  multi_get_random_param_new(Rest, Dict, Value, ObjRest, SetSize, [Param|Acc]).

get_random_param_new(Key, Dict, Type, Value, Obj, SetSize) ->
  Params = dict:fetch(Type, Dict),
  Num = random:uniform(length(Params)),
  BKey = list_to_binary(integer_to_list(Key)),
  NewVal = case Value of
             Value when is_integer(Value) ->
               integer_to_list(Value);
             Value when is_binary(Value) ->
               binary_to_list(Value)
           end,
  case Type of
    riak_dt_pncounter ->
      [{{BKey, Type, <<"bucket">>}, lists:nth(Num, Params), 1}];
    riak_dt_lwwreg ->
      [{{BKey, Type, <<"bucket">>}, assign, NewVal}];
    Type when Type == riak_dt_orset; Type == crdt_orset ->
      Set =
        case Obj of
          undefined ->
            [];
          Obj ->
            antidotec_set:value(Obj)
        end,
      %%Op = lists:nth(Num, Params),
      NewOp = case length(Set) =< SetSize of
                true ->
                  add;
                false ->
                  remove
              end,
      case NewOp of
        remove ->
          case Set of
            [] ->
              [{{BKey, Type, <<"bucket">>}, add_all, [NewVal]}];
            Set ->
              [{{BKey, Type, <<"bucket">>}, remove_all, [lists:nth(random:uniform(length(Set)), Set)]}]
          end;
        _ ->
          [{{BKey, Type, <<"bucket">>}, add_all, [NewVal]}]
      end
  end.

get_random_param(Dict, Type, Value) ->
  Params = dict:fetch(Type, Dict),
  random:seed(now()),
  Num = random:uniform(length(Params)),
  case Type of
    riak_dt_pncounter ->
      {antidotec_counter, lists:nth(Num, Params), 1};
    riak_dt_orset ->
      {antidotec_set, lists:nth(Num, Params), Value}
  end.

get_random_param(Dict, Type, Value, Obj, SetSize) ->
  Params = dict:fetch(Type, Dict),
  Num = random:uniform(length(Params)),
  case Type of
    riak_dt_pncounter ->
      {antidotec_counter, lists:nth(Num, Params), 1};
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
            [H | _T] -> {antidotec_set, remove, H}
          end;
        _ ->
          {antidotec_set, NewOp, Value}
      end
  end.

report_staleness(true, CT, CurTime) ->
    SS1 = binary_to_term(CT), %% Binary to dict
    SS = binary_to_list(CT),
    lager:info("CT = ",[CT]),
    lager:info("Bynary to term = ",[SS1]),
    lager:info("Bynary to list = ",[SS]),

    %% Here it is assumed the stable snapshot has entries for all remote DCs
%%    SSL = lists:keysort(1, dict:to_list(SS)),
    SSL = lists:keysort(1, SS),
    Staleness = lists:map(fun({_Dc, Time}) ->
                                  max(1, CurTime - Time) %% it should be max(0, ..), but 0 is causing some crash in stats generation
                          end, SSL),
    HistName = atom_to_list(staleness),
    report_staleness_rec(Staleness, HistName, 1);

report_staleness(_,_,_) ->
     ok.

report_staleness_rec([],_,_) -> ok;
report_staleness_rec([H|T], HistName, Iter) ->
    Op=list_to_atom(string:concat(HistName, integer_to_list(Iter))),
    folsom_metrics:notify({latencies, {Op, Op}}, H),
    folsom_metrics:notify({units, {Op, Op}}, {inc, 1}),
    report_staleness_rec(T, HistName, Iter+1).

now_microsec() ->    
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.                                                  

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


%% @doc generate NumReads unique keys using the KeyGen
generate_keys(NumKeys, KeyGen) ->
  Seq = lists:seq(1, NumKeys),
  S = lists:foldl(fun(_, Set) ->
    N = unikey(KeyGen, Set),
    sets:add_element(N, Set)
                  end, sets:new(), Seq),
  sets:to_list(S).


unikey(KeyGen, Set) ->
  R = KeyGen(),
  case sets:is_element(R, Set) of
    true ->
      unikey(KeyGen, Set);
    false ->
      R
  end.


random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).
