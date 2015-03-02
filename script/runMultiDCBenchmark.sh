#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc, erl|pb"
	exit
else
	AllSystemNodes=$1
    SystemNodesArray=($AllSystemNodes)
	Cookie=$2
	NumberDC=$3
	NodesPerDC=$4
    NodesToUse=$((NumberDC * NodesPerDC))
	AllNodes=${SystemNodesArray[@]:0:$NodesToUse}
    AllNodes=`echo ${AllNodes[@]}`
    echo "Using" $AllNodes
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
./script/deployMultiDCs.sh "$AllNodes" $Cookie $NumberDC $NodesPerDC

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
