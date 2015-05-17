#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name haha@localhost

main(_SringNodes) -> 
    Cookie='antidote',
    DCPerRing = 1,

    true = erlang:set_cookie(node(), Cookie),
    %DCInfo=[{'127.0.0.1', 10017}, {'127.0.0.1', 10027}],
    Nodes=[{'dev1@127.0.0.1', 10017} , {'dev2@127.0.0.1', 10027} ],

    DCs = startListening(Nodes),
    connect_each(Nodes, DCPerRing, 1, DCs).

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
    lists:foldl(fun({Node, Port}, Acc) ->
                        io:format("Calling node ~w ~n",[Node]),
                        {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[Port]),
                        io:format("Datacenter ~w ~n", [DC]),
                        Acc++[DC]
                end,[],Nodes).
    
                        

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
    [{Node, _Port}|Rest] ->
            io:format("Connect node ~w to ~w ~n", [Node, OtherDCs]),
        ok = rpc:call(Node, inter_dc_manager, add_list_dcs,[OtherDCs]),
        connect(Rest, OtherDCs)
    end.


