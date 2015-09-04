-module(connectDCs).

-export([listenAndConnect/1, connect/6
	]).

-define(LISTEN_PORT, 8091).

listenAndConnect(StringNodes) -> 
    Temp = [list_to_atom(StringNode) || StringNode <- StringNodes],
    Cookie = hd(Temp),
    DCPerRing = list_to_integer(atom_to_list(hd(tl(Temp)))),
    Branch = hd(tl(tl(Temp))),
    Nodes = tl(tl(tl(Temp))),
    io:format("Nodes ~w ~n", [Nodes]),
    io:format("Branch from erl ~w ~n", [Branch]),
    
    true = erlang:set_cookie(node(), Cookie),
    NumDCs = length(Nodes) div DCPerRing,
    io:format("NumDCs ~w ~n", [NumDCs]),

    CookieNodes = addCookie(Nodes, Cookie, []),
    wait_ready_nodes(CookieNodes),
    HeadNodes = keepnth(CookieNodes, DCPerRing, 0, []), 
    HeadNodesIp = keepnth(Nodes, DCPerRing, 0, []), 
    Ports = lists:seq(?LISTEN_PORT, ?LISTEN_PORT + NumDCs -1),
    DCInfo = addPort(HeadNodes, Ports, []),
    DCList = startListeners(DCInfo,Branch,[]),
    connect_each(CookieNodes, DCPerRing, 1, DCInfo, HeadNodesIp, Ports, DCList, Branch).

wait_ready_nodes([]) ->
    true;
wait_ready_nodes([Node|Rest]) ->
    case check_ready(Node) of
	true ->
	    wait_ready_nodes(Rest);
	false ->
	    timer:sleep(100),
	    wait_ready_nodes([Node|Rest])
    end.


check_ready(Node) ->
    io:format("Checking if node ~w is ready ~n", [Node]),
    try
	case rpc:call(Node,clocksi_vnode,check_tables_ready,[]) of
	    true ->
		case rpc:call(Node,clocksi_readitem_fsm,check_servers_ready,[]) of
		    true ->
			case rpc:call(Node,materializer_vnode,check_tables_ready,[]) of
			    true ->
				io:format("Node ~w is ready! ~n", [Node]),
				true;
			    false ->
				io:format("Node ~w is not ready ~n", [Node]),
				false
			end;
		    false ->
			io:format("Checking if node ~w is ready ~n", [Node]),
			false
		end;
	    false ->
		io:format("Checking if node ~w is ready ~n", [Node]),
		false
	end
    catch
        _:Reason ->
	    io:format(standard_error, "RPC failed to check nodes ready so skipping this, reason: ~p", [Reason]),
	    true
    end.



%% connect(NodeList) ->
%%     [LocalSize|Rest]=NodeList,
%%     {Nodes, Listeners} =lists:split(list_to_integer(LocalSize), Rest),
%%     FullNodes = addCookie(Nodes, antidote, []),
%%     Ports= lists:duplicate(length(Listeners), ?LISTEN_PORT),
%%     ListenerAddrs = addPort(Listeners, Ports, []),
%%     connect(FullNodes, ListenerAddrs).


%% startListener([Node]) ->
%%     {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[?LISTEN_PORT]),
%%     io:format("Datacenter ~w ~n", [DC]),
%%     DC.


%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%
addCookie([], _, Adder) ->
    Adder;
addCookie([Node|RestNodes], Cookie, Adder) ->
    NodeAddr = case is_atom(Node) of 
                true ->
                    list_to_atom(atom_to_list(Cookie) ++ "@" ++ atom_to_list(Node));
                false ->
                    list_to_atom(atom_to_list(Cookie) ++ "@" ++ Node)
                end,
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

startListeners([], _Branch, Acc) ->
    Acc;
startListeners([{Node, Port}|Rest], Branch, Acc) ->
    {ok, DC} = case Branch of
		   pubsub_bench ->
		       rpc:call(Node, inter_dc_manager, get_descriptor, []);
		   pubsub_bench_log ->
		       rpc:call(Node, inter_dc_manager, get_descriptor, []);
		   pubsub_log_strong ->
		       rpc:call(Node, inter_dc_manager, get_descriptor, []);
		   _ ->
		       rpc:call(Node, inter_dc_manager, start_receiver,[Port])
	       end,
    io:format("Datacenter ~w ~n", [DC]),
    startListeners(Rest, Branch, [DC | Acc]).

connect_each([], _DCPerRing, _Acc, _AllDCs, _, _, _, _) ->
    ok;
connect_each(Nodes, DCPerRing, Acc, AllDCs, Allips, Allports, DCList, Branch) ->
    {DCNodes, Rest} = lists:split(DCPerRing, Nodes),
    OtherDCList = DCList -- [lists:nth(Acc, DCList)],
    OtherDCs = AllDCs -- [lists:nth(Acc, AllDCs)],
    OtherIps = Allips -- [lists:nth(Acc, Allips)],
    OtherPorts = Allports -- [lists:nth(Acc, Allports)],
    case OtherDCs of 
	[] ->
	    io:format("Empty dc, no need to connect!~n");
	_ ->
    	    connect(DCNodes, OtherDCs, OtherIps, OtherPorts, OtherDCList, Branch)
    end,
    connect_each(Rest, DCPerRing, Acc+1, AllDCs, Allips, Allports, DCList, Branch).

connect(Nodes, OtherDCs, OtherIps, OtherPorts, OtherDCList, Branch) ->
    case Nodes of
	[] ->
		ok;
	[Node|Rest] ->
	    io:format("Connect node ~w to ~w ~n", [Node, OtherDCs]),
	    lists:foldl(fun(_DC, Acc) ->
				io:format("Acc ~w ~n", [Acc]),
				Ip = lists:nth(Acc, OtherIps),
				Port = lists:nth(Acc, OtherPorts),
				OtherDC = lists:nth(Acc, OtherDCList),
				io:format("Connecting a dc ip ~w, port ~w or ~w ~n", [Ip,Port,OtherDC]),
				%% ok = rpc:call(Node, inter_dc_manager, add_dc,[{DC, {atom_to_list(Ip), Port}}]),
				case Branch of
				    pubsub_bench ->
					ok = rpc:call(Node, inter_dc_manager, observe_dc,[OtherDC]);
				    pubsub_bench_log ->
					ok = rpc:call(Node, inter_dc_manager, observe_dc,[OtherDC]);
				    pubsub_log_strong ->
					ok = rpc:call(Node, inter_dc_manager, observe_dc,[OtherDC]);
				    _ ->
					ok = rpc:call(Node, inter_dc_manager, add_dc,[OtherDC])
				end,
				Acc + 1
			end, 1, OtherDCs),
	    connect(Rest, OtherDCs, OtherIps, OtherPorts, OtherDCList, Branch)
    end.
