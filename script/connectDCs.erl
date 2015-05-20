-module(connectDCs).

-export([listenAndConnect/1, startListener/1,
         connect/1]).

-define(LISTEN_PORT, 8091).

listenAndConnect(StringNodes) -> 
    Temp = [list_to_atom(StringNode) || StringNode <- StringNodes],
    Cookie = hd(Temp),
    DCPerRing = list_to_integer(atom_to_list(hd(tl(Temp)))),
    Nodes = tl(tl(Temp)),
    io:format("Nodes ~w ~n", [Nodes]),

    true = erlang:set_cookie(node(), Cookie),
    NumDCs = length(Nodes) div DCPerRing,
    io:format("NumDCs ~w ~n", [NumDCs]),

    CookieNodes = addCookie(Nodes, Cookie, []),
    HeadNodes = keepnth(CookieNodes, DCPerRing, 0, []), 
    Ports = lists:seq(?LISTEN_PORT, ?LISTEN_PORT + NumDCs -1),
    DCInfo = addPort(HeadNodes, Ports, []),
    startListeners(DCInfo),
    connect_each(CookieNodes, DCPerRing, 1, DCInfo).

connect(NodeList) ->
    [LocalSize|Rest]=NodeList,
    {Nodes, Listeners} =lists:split(list_to_integer(LocalSize), Rest),
    FullNodes = addCookie(Nodes, antidote, []),
    Ports= lists:duplicate(length(Listeners), ?LISTEN_PORT),
    ListenerAddrs = addPort(Listeners, Ports, []),
    case FullNodes of
        [] ->
                ok;
        [Node|Others] ->
                io:format("Connect node ~w to ~w ~n", [Node, ListenerAddrs]),
                case rpc:call(Node, inter_dc_manager, add_list_dcs,[ListenerAddrs]) of
                    ok -> 
                        io:format("Successfully connected");
                    _ ->
                        io:format("*****Failure connecting.******")
                end,
                connect([Others, ListenerAddrs])
    end.


startListener([NodeStr]) ->
    Node=list_to_atom(NodeStr),
    {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[?LISTEN_PORT]),
    io:format("Datacenter ~w ~n", [DC]).


%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%
addCookie([], _, Adder) ->
    Adder;
addCookie([Node|RestNodes], Cookie, Adder) ->
    NodeAddr = list_to_atom(atom_to_list(Cookie) ++ "@" ++ atom_to_list(Node)),
    addCookie(RestNodes, Cookie, Adder ++ [NodeAddr]).

addPort([], [], Adder) ->
    Adder;
addPort([Node|RestNodes], [Port|RestPorts], Adder) ->
    addPort(RestNodes, RestPorts, Adder ++ [{Node, Port}]).

keepnth([], _, _, AccList) ->
    AccList;
keepnth([First|Rest], Length, AccNum, AccList) ->
    case AccNum rem Length of
	0 -> keepnth(Rest, Length, AccNum+1, AccList++[First]);
	_ -> keepnth(Rest, Length, AccNum+1, AccList)
    end.

startListeners([]) ->
    ok;
startListeners([{Node, Port}|Rest]) ->
    {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[Port]),
	io:format("Datacenter ~w ~n", [DC]),
	startListeners(Rest).

connect_each([], _DCPerRing, _Acc, _AllDCs) ->
    ok;
connect_each(Nodes, DCPerRing, Acc, AllDCs) ->
    {DCNodes, Rest} = lists:split(DCPerRing, Nodes),
    OtherDCs = AllDCs -- [lists:nth(Acc, AllDCs)],
    case OtherDCs of 
	[] ->
	    io:format("Empty dc, no need to connect!~n");
	_ ->
    	    connect(DCNodes, OtherDCs)
    end,
    connect_each(Rest, DCPerRing, Acc+1, AllDCs).

connect(Nodes, OtherDCs) ->
    case Nodes of
	[] ->
		ok;
	[Node|Rest] ->
	        io:format("Connect node ~w to ~w ~n", [Node, OtherDCs]),
		ok = rpc:call(Node, inter_dc_manager, add_list_dcs,[OtherDCs]),
		connect(Rest, OtherDCs)
    end.
