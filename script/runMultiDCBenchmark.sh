#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: all_nodes, nodes_for_this_bench, cookie, if_to_connect_nodes, erl|pb"
	exit
else
	AllSystemNodes=$1
	AllNodes=$2
	echo $AllNodes
	Cookie=$3
	ConnectDCs=$4
    if [ $5 -eq "erl" ]; then
        BenchmarkType=0
    elif [ $5 -eq "pb" ]; then
        BenchmarkType=1
    else
        echo "Wrong benchmark type!"
        exit
    fi
fi
./script/stopNodes.sh "$AllSystemNodes" 
./script/deployMultiDCs.sh "$AllNodes" $Cookie $ConnectDCs

##Replace benchmark configuration to include nodes
if [ $BenchmarkType -eq "erl" ]; then
    FileName="examples/antidote.config"
    ./script/changeErlConfig.sh "$AllNodes" $Cookie $FileName
else
    FileName="examples/antidote_pb.config"
    ./script/changePBConfig.sh "$AllNodes" $Cookie $FileName
fi
sudo ./basho_bench $FileName 
