#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name haha@localhost

main(StringNodes) -> 
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
    Ports = lists:seq(8091, 8090 + NumDCs),
    DCInfo = combineLists(HeadNodes, Ports, []),
    startListening(DCInfo),
    connect_each(CookieNodes, DCPerRing, 1, DCInfo).

addCookie([], _, Adder) ->
    Adder;
addCookie([Node|RestNodes], Cookie, Adder) ->
    NodeAddr = list_to_atom(atom_to_list(Cookie) ++ "@" ++ atom_to_list(Node)),
    addCookie(RestNodes, Cookie, Adder ++ [NodeAddr]).

combineLists([], [], Adder) ->
    Adder;
combineLists([Node|RestNodes], [Port|RestPorts], Adder) ->
    combineLists(RestNodes, RestPorts, Adder ++ [{Node, Port}]).

keepnth([], _, _, AccList) ->
    AccList;
keepnth([First|Rest], Length, AccNum, AccList) ->
    case AccNum rem Length of
	0 -> keepnth(Rest, Length, AccNum+1, AccList++[First]);
	_ -> keepnth(Rest, Length, AccNum+1, AccList)
    end.

startListening(Nodes) ->
    case Nodes of
	[] ->
	    ok;
	[{Node, Port}|Rest] ->
    	    {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[Port]),
	    io:format("Datacenter ~w ~n", [DC]),
	    startListening(Rest)
    end.

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

