#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: all_nodes, nodes_for_this_bench, cookie, if_to_connect_nodes, erl|pb"
	exit
else
	AllSystemNodes=$1
	AllNodes=$2
	Cookie=$3
	ConnectDCs=$4
	DCPerRing=$6
	echo $5
    if [ "$5" = "erl" ]; then
	echo "Benchmark erl"
        BenchmarkType=0
    elif [ "$5" = "pb" ]; then
	echo "Benchmark pb"
        BenchmarkType=1
    else
        echo "Wrong benchmark type!"
        exit
    fi
fi
./script/stopNodes.sh "$AllSystemNodes" 
./script/deployMultiDCs.sh "$AllNodes" $Cookie $ConnectDCs $DCPerRing

##Replace benchmark configuration to include nodes
if [ $BenchmarkType -eq 0 ]; then
    FileName="examples/antidote.config"
    ./script/changeErlConfig.sh "$AllNodes" $Cookie $FileName
else
    FileName="examples/antidote_pb.config"
    ./script/changePBConfig.sh "$AllNodes" $Cookie $FileName
fi

LoadFile="loadfile.config"
./script/createLoadFile.sh $FileName $LoadFile
echo "Loading phase..."
sudo ./basho_bench "$LoadFile"
echo "Benchmarking phase..."
sudo ./basho_bench $FileName 
