-module(lasp_bench_driver_unir).

-export([new/1,
         run/4,
         terminate/2]).

-record(state, {nodes, runner, num_ops}).

-include("lasp_bench.hrl").

new(_Id) ->
    Nodes = case os:getenv("NODES") of
        false ->
            exit({error, no_nodes});
        NValue ->
            lists:map(fun(X) ->
                list_to_atom(X)
            end, string:tokens(NValue, ","))
    end,

    RunnerNode = case os:getenv("RUNNER") of
        false ->
            exit({error, no_runner});
        RValue ->
            RNode = list_to_atom(RValue),
            true = net_kernel:connect(RNode),
            RNode
    end,

    ConnectedNodes = lists:map(fun(Node) ->
        case net_kernel:connect(Node) of
            true ->
                Node;
            false ->
                exit({error, {not_connected, Node}})
        end
    end, Nodes),

    {ok, #state{runner=RunnerNode, nodes=ConnectedNodes, num_ops=0}}.

run(fsm_get, KeyGen, _ValueGen, #state{num_ops=NumOps}=State) ->
    Key = KeyGen(),
    Node = random_node(State),

    case rpc:call(Node, unir, fsm_get, [Key]) of
        {ok, _} ->
            {ok, State#state{num_ops=NumOps+1}};
        Error ->
            {error, Error, State#state{num_ops=NumOps+1}}
    end;

run(fsm_put, KeyGen, ValueGen, #state{num_ops=NumOps}=State) ->
    Key = KeyGen(),
    Value = ValueGen(),
    Node = random_node(State),

    case rpc:call(Node, unir, fsm_put, [Key, Value]) of
        {ok, _} ->
            {ok, State#state{num_ops=NumOps+1}};
        Error ->
            {error, Error, State#state{num_ops=NumOps+1}}
    end;

run(Command, _KeyGen, _ValueGen, #state{num_ops=NumOps}=State) ->
    Node = random_node(State),

    case rpc:call(Node, unir, Command, []) of
        %% command
        ok ->
            {ok, State#state{num_ops=NumOps+1}};
        %% sync_command
        {pong, _} ->
            {ok, State#state{num_ops=NumOps+1}};
        %% error
        Error ->
            {error, Error, State#state{num_ops=NumOps+1}}
    end.

terminate(_Reason, #state{num_ops=NumOps, runner=_RunnerNode}) ->
    Pid = global:whereis_name(runner),
    erlang:send(Pid, {bench_operations, NumOps}),
    ok.

%% @private
random_node(#state{nodes=Nodes}) ->
    lists:nth(rand:uniform(length(Nodes)), Nodes).