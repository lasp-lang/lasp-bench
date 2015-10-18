-module(connectDCs).

-export([listenAndConnect/1, connect/6
	]).

-define(LISTEN_PORT, 8091).

listenAndConnect(StringNodes) -> 
    Temp = [list_to_atom(StringNode) || StringNode <- StringNodes],
    Cookie = hd(Temp),
    DCPerRing = list_to_integer(atom_to_list(hd(tl(Temp)))),
    Branch = hd(tl(tl(Temp))),
    BenchmarkFile = hd(tl(tl(tl(Temp)))),
    Nodes = tl(tl(tl(tl(Temp)))),
    io:format("Nodes ~w ~n", [Nodes]),
    io:format("Branch from erl ~w ~n", [Branch]),
    
    true = erlang:set_cookie(node(), Cookie),
    NumDCs = length(Nodes) div DCPerRing,
    io:format("NumDCs ~w ~n", [NumDCs]),

    CookieNodes = addCookie(Nodes, Cookie, []),
    IsPubSub = case re:run(atom_to_list(Branch),"pubsub") of
		   {match, _} ->
		       true;
		   nomatch ->
		       false
	       end,
    IsPartial = case re:run(atom_to_list(Branch),"partial") of
		    {match, _} ->
			true;
		    nomatch ->
			false
		end,
    wait_ready_nodes(CookieNodes, IsPubSub, IsPartial),
    HeadNodes = keepnth(CookieNodes, DCPerRing, 0, []), 
    HeadNodesIp = keepnth(Nodes, DCPerRing, 0, []), 
    case IsPartial of
	true ->
	    ReplicationFactor = case re:run(atom_to_list(BenchmarkFile),"_rep") of
				    {match, {Pos,_Len}} ->
					list_to_integer(string:substr(atom_to_list(BenchmarkFile),Pos+5,1));
				    nomatch ->
					NumDCs
				end,
	    io:format("Replication factor ~w", [ReplicationFactor]),
	    %% Ports = createPorts(8050,NumDCs,NumDCs,[]),
	    Ports = createPorts(?LISTEN_PORT,NumDCs,NumDCs,[]),
	    DCInfo = combineLists(HeadNodesIp, Ports, []),
	    startListening(HeadNodes,Ports,1),
	    {DCInfoIds,_} = add_ids(DCInfo),
	    connect_each_partial(CookieNodes, DCPerRing, 1, DCInfoIds),
	    startSenders(HeadNodes),
	    startPerNodeConnection(CookieNodes),
	    setReplicationFunction(HeadNodes,NumDCs,ReplicationFactor);
	false ->
	    Ports =  lists:seq(?LISTEN_PORT, ?LISTEN_PORT + NumDCs -1),
	    DCInfo = addPort(HeadNodes, Ports, []),
	    DCList = startListeners(DCInfo,Branch,[]),
	    connect_each(CookieNodes, DCPerRing, 1, DCInfo, HeadNodesIp, Ports, DCList, Branch)
    end,
    %% Sleep for some time to let the DCs stablize
    timer:sleep(30000).


createPorts(_StartInt, _NumDcs, 0, Acc) ->
    Acc;
createPorts(StartInt,NumDcs,Remainder,Acc) ->
    NewAcc = Acc ++ [lists:seq(StartInt,StartInt + NumDcs)],
    createPorts(StartInt + NumDcs + 1, NumDcs, Remainder - 1, NewAcc).


wait_ready_nodes([], _IsPubSub, _IsPartial) ->
    true;
wait_ready_nodes([Node|Rest], IsPubSub, IsPartial) ->
    case check_ready(Node) of
	true ->
	    case IsPubSub of 
		true ->
		    wait_until_registered(Node, inter_dc_pub),
		    wait_until_registered(Node, inter_dc_log_reader_response),
		    wait_until_registered(Node, inter_dc_log_reader_query),
		    wait_until_registered(Node, inter_dc_sub);
		false ->
		    case IsPartial of
			true ->
			    wait_until_registered(Node, meta_data_sender);
			false ->
			    wait_until_registered(Node, inter_dc_manager)
		    end
	    end,
	    wait_ready_nodes(Rest,IsPubSub, IsPartial);
	false ->
	    timer:sleep(100),
	    wait_ready_nodes([Node|Rest],IsPubSub,IsPartial)
    end.


check_ready(Node) ->
    io:format("Checking if node ~w is ready ~n", [Node]),
    try
	case rpc:call(Node,clocksi_vnode,check_tables_ready,[],5000) of
	    true ->
		case rpc:call(Node,clocksi_readitem_fsm,check_servers_ready,[],5000) of
		    true ->
			case rpc:call(Node,materializer_vnode,check_tables_ready,[],5000) of
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

combineLists([],[],Acc) ->
    Acc;
combineLists([Node|RestNodes],[Ports|RestPorts],Acc) ->
    NodePort = lists:foldl(fun(Port,LAcc) ->
				   LAcc ++ [{Node,Port}]
			   end,[],Ports),
    combineLists(RestNodes,RestPorts,Acc ++ [NodePort]).

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
    io:format("Getting descriptor"),
    {ok, DC} = case re:run(atom_to_list(Branch),"pubsub") of
		   {match, _} ->
		       rpc:call(Node, inter_dc_manager, get_descriptor, []);
		   nomatch ->
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
				case re:run(atom_to_list(Branch),"pubsub") of
				    {match, _} ->
					ok = rpc:call(Node, inter_dc_manager, observe_dc_sync,[OtherDC]);
				    nomatch ->
					ok = rpc:call(Node, inter_dc_manager, add_dc,[OtherDC])
				end,
				Acc + 1
			end, 1, OtherDCs),
	    connect(Rest, OtherDCs, OtherIps, OtherPorts, OtherDCList, Branch)
    end.


%% These functions are copied from https://github.com/basho/riak_test/blob/master/src/rt.erl

% Waits until a certain registered name pops up on the remote node.
wait_until_registered(Node, Name) ->
    io:format("Wait until ~p is up on ~p~n", [Name, Node]),

    F = fun() ->
                Registered = rpc:call(Node, erlang, registered, []),
                lists:member(Name, Registered)
        end,
    case wait_until(F) of
        ok ->
            ok;
        _ ->
            io:format("The server with the name ~p on ~p is not coming up.~n",
                       [Name, Node])
    end.

wait_until(Fun) when is_function(Fun) ->
    MaxTime = 20000,
    Delay = 1000,
    Retry = MaxTime div Delay,
    wait_until(Fun, Retry, Delay).

wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Res = Fun(),
    case Res of
        true ->
            ok;
        _ when Retry == 1 ->
            {fail, Res};
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry-1, Delay)
    end.


%% Partial rep stuff

startListening(Nodes,Ports,DcNum) ->
    case Nodes of
	[] ->
	    ok;
	[Node|Rest] ->
	    [[Port|RestPort]|Rem] = Ports,
	    NewRestPort = RestPort -- [lists:nth(DcNum,RestPort)],
	    io:format("connecting to node ~w, rep ports ~w, read port ~w~n", [Node,NewRestPort,Port]),
	    {ok,DCRepPorts,DC} = rpc:call(Node,antidote_sup,start_recvrs,[internet,DcNum,NewRestPort,Port]),
	    io:format("Datacenter listening: ~w, ~w ~n", [DCRepPorts,DC]),
	    startListening(Rest,Rem,DcNum+1)
    end.

startSenders(Nodes) ->
    case Nodes of
	[] ->
	    ok;
	[Node|Rest] ->
	    io:format("Starting senders at node ~w~n",[Node]),
	    ok = rpc:call(Node,antidote_sup,start_senders,[]),
	    startSenders(Rest)
    end.


startPerNodeConnection(Nodes) ->
    case Nodes of
	[] ->
	    ok;
	[Node|Rest] ->
	    io:format("Starting ext read connection node ~w~n",[Node]),
	    ok = rpc:call(Node,antidote_sup,start_ext_read_connection,[]),
	    startPerNodeConnection(Rest)
    end.



%% startListening(Nodes) ->
%%     case Nodes of
%% 	[] ->
%% 	    ok;
%% 	[{Node, Port}|Rest] ->
%%     	    {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[Port]),
%% 	    io:format("Datacenter ~w ~n", [DC]),
%% 	    startListening(Rest)
%%     end.


connect_each_partial([], _DCPerRing, _Acc, _AllDCs) ->
    ok;
connect_each_partial(Nodes, DCPerRing, Acc, AllDCs) ->
    {DCNodes, Rest} = lists:split(DCPerRing, Nodes),
    OtherDCs = AllDCs -- [lists:nth(Acc, AllDCs)],
    case OtherDCs of 
	[] ->
	    io:format("Empty dc, no need to connect!~n");
	_ ->
    	    connect_partial(DCNodes, OtherDCs, Acc)
    end,
    connect_each_partial(Rest, DCPerRing, Acc+1, AllDCs).

connect_partial(Nodes, OtherDCs, DcNum) ->
    case Nodes of
	[] ->
	    ok;
	[Node|_Rest] ->
	    ExtDcs = get_external_dcs(DcNum,OtherDCs),
	    io:format("Connect node ~w to ~w ~n", [Node, ExtDcs]),
	    ok = rpc:call(Node, inter_dc_manager, add_list_dcs, [ExtDcs]),
	    ExtReadDcs = get_external_read_dcs(OtherDCs),
	    io:format("Connect read node ~w to ~w ~n", [Node, ExtReadDcs]),
	    ok = rpc:call(Node, inter_dc_manager, add_list_read_dcs, [ExtReadDcs])
	    %%connect(Rest, OtherDCs,DcNum)
    end.

setReplicationFunction(DcList,NumDcs,ReplicationFactor) ->
    Function = create_biased_key_function(ReplicationFactor,NumDcs),
    lists:foldl(fun(Dc,Id) ->
			ok = rpc:call(Dc,inter_dc_manager,set_replication_fun,[Function,Id]),
			Id + 1
		end, 0, DcList).

add_ids(DoubleDcList) ->
    lists:foldl(fun(DcList,{Acc,Id}) ->
			ADC = lists:foldl(fun(Dc,Acc2) ->
					    Acc2 ++ [{Id,Dc}]
					  end,[],DcList),
			{Acc ++ [ADC],Id+1}
		end,{[],1},DoubleDcList).


get_external_dcs(DcNum,OtherDCs) ->
    io:format("the other dcs: ~w~n", [OtherDCs]),
    lists:foldl(fun(DcPorts,Acc) ->
			Acc ++ [lists:nth(DcNum+1,DcPorts)]
		end,[],OtherDCs).

get_external_read_dcs(OtherDCs) ->
    lists:foldl(fun(DcPorts,Acc) ->
			Acc ++ [hd(DcPorts)]
		end,[],OtherDCs).

% Should return a list where each value is a sigle element tuple with the Dc number
create_biased_key_function(ReplicationFactor,NumDcs) ->
    fun(Key) ->
	    
	    FirstDc = case Key rem NumDcs of
			  0 ->
			      NumDcs;
			  Else ->
			      Else
		      end,
	    ListFun = fun(Self,Count,Acc) ->
			      case Count of
				  ReplicationFactor ->
				      Acc;
				  _ ->
				      case (FirstDc + Count) rem NumDcs of
					  0 ->
					      Self(Self,Count + 1,Acc ++ [{NumDcs}]);
					  Other ->
					      Self(Self,Count+1,Acc ++ [{Other}])
				      end
			      end
		      end,
	    ListFun(ListFun,1,[{FirstDc}])
    end.
    
