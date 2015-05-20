-module(connDCFun).

-export([startListening/1,
         connect/2]).

startListening(Nodes) ->
    case Nodes of
	[] ->
	    ok;
	[{Node, Port}|Rest] ->
    	    {ok, DC} = rpc:call(Node, inter_dc_manager, start_receiver,[Port]),
	    io:format("Datacenter ~w ~n", [DC]),
	    startListening(Rest)
    end.


connect(Nodes, OtherDCs) ->
    case Nodes of
	[] ->
		ok;
	[Node|Rest] ->
	        io:format("Connect node ~w to ~w ~n", [Node, OtherDCs]),
		ok = rpc:call(Node, inter_dc_manager, add_list_dcs,[OtherDCs]),
		connect(Rest, OtherDCs)
    end.

