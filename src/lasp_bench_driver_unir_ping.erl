-module(lasp_bench_driver_unir_ping).

-export([new/1,
         run/4]).

-record(state, {nodes}).

new(_Id) ->
    Nodes = case os:getenv("NODES") of
        false ->
            exit({error, no_nodes});
        Value ->
            lists:map(fun(X) ->
                list_to_atom(X)
            end, string:tokens(Value, ","))
    end,

    lager:info("Attempting to connect to nodes: ~p", [Nodes]),

    ConnectedNodes = lists:map(fun(Node) ->
        lager:info("Connecting to node: ~p", [Node]),

        case net_kernel:connect(Node) of
            true ->
                Node;
            false ->
                lager:info("Couldn't connect to node: ~p", [Node]),
                exit({error, {not_connected, Node}})
        end
    end, Nodes),

    {ok, #state{nodes=ConnectedNodes}}.

run(Command, _KeyGen, _ValueGen, State) ->
    Node = random_node(State),

    case rpc:call(Node, unir, Command, []) of
        %% command
        ok ->
            {ok, State};
        %% sync_command
        {pong, _} ->
            {ok, State};
        %% error
        Error ->
            {error, Error, State}
    end.

%% @private
random_node(#state{nodes=Nodes}) ->
    lists:nth(random:uniform(length(Nodes)), Nodes).