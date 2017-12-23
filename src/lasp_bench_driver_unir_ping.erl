-module(lasp_bench_driver_unir_ping).

-export([new/1,
         run/4]).

-record(state, {nodes}).

new(_Id) ->
    lager:info("Attempting to connect to nodes."),

    Nodes = ['node_1@parrhesia.localdomain', 'node_2@parrhesia.localdomain', 'node_3@parrhesia.localdomain'],

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
        ok ->
            {ok, State};
        Error ->
            {error, Error, State}
    end.

%% @private
random_node(#state{nodes=Nodes}) ->
    lists:nth(random:uniform(length(Nodes)), Nodes).