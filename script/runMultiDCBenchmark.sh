#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: all_nodes, nodes_for_this_bench, cookie, if_to_connect_nodes"
	exit
else
	AllSystemNodes=$1
	AllNodes=$2
	echo $AllNodes
	Cookie=$3
	ConnectDCs=$4
fi
./script/stopNodes.sh "$AllSystemNodes" 
./script/deployMultiDCs.sh "$AllNodes" $Cookie $ConnectDCs

##Replace benchmark configuration to include nodes
File="examples/antidote_pb.config"
./script/changeConfig.sh "$AllNodes" $Cookie $File
sudo ./basho_bench $File 
