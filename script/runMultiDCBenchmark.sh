#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc, connect_dc_or_not, erl|pb, bench_parallel"
    exit
else
    AllSystemNodes=$1
    SystemNodesArray=($AllSystemNodes)
    Cookie=$2
    NumberDC=$3
    NodesPerDC=$4
    BenchParallel=$5
    BenchNodes=`cat script/allnodesbench`
    NodesToUse=$((NumberDC * NodesPerDC))
    AllNodes=${SystemNodesArray[@]:0:$NodesToUse}
    AllNodes=`echo ${AllNodes[@]}`
    ConnectDCs=$5
    echo "Using" $AllNodes ", will connect DCs:" $ConnectDCs
    if [ "$6" = "erl" ]; then
	echo "Benchmark erl"
        BenchmarkType=0
    elif [ "$6" = "pb" ]; then
	echo "Benchmark pb"
        BenchmarkType=1
    else
        echo "Wrong benchmark type!"
        exit
    fi
fi

./script/stopNodes.sh "$AllSystemNodes" 
./script/deployMultiDCs.sh "$AllNodes" $Cookie $ConnectDCs $NodesPerDC

# Run the benchmarks in parallel
# This is not a good way to do this, should be implemented inside basho bench
for Node in $BenchNodes; do
    for I in $(seq 1 $BenchParallel); do
	ssh root@$Node /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmark.sh $4 $BenchmarkType $I &
    done
done
wait


#./script/runSimpleBenchmark.sh $4 $BenchmarkType
