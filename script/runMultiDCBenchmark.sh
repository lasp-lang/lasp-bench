#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc, bench_nodes_per_dc, connect_dc_or_not, erl|pb, bench_parallel"
    exit
else
    AllSystemNodes=$1
    SystemNodesArray=($AllSystemNodes)
    Cookie=$2
    NumberDC=$3
    NodesPerDC=$4
    BenchNodesPerDC=$5
    BenchNodes=`cat script/allnodesbench`
    NodesToUse=$((NumberDC * NodesPerDC))
    AllNodes=${SystemNodesArray[@]:0:$NodesToUse}
    AllNodes=`echo ${AllNodes[@]}`
    ConnectDCs=$6
    echo "Using" $AllNodes ", will connect DCs:" $ConnectDCs
    if [ "$7" = "erl" ]; then
	echo "Benchmark erl"
        BenchmarkType=0
    elif [ "$7" = "pb" ]; then
	echo "Benchmark pb"
        BenchmarkType=1
    else
        echo "Wrong benchmark type!"
        exit
    fi
    BenchParallel=$8
fi

./script/stopNodes.sh "$AllSystemNodes" 
./script/deployMultiDCs.sh "$AllNodes" $Cookie $ConnectDCs $NodesPerDC

cat script/allnodes > ./tmpnodelist
cat script/allnodesbench > ./tmpnodelistbench
for DCNum in $(seq 1 $NumberDC); do
    NodeArray[$DCNum]=`head -$NodesPerDC tmpnodelist`
    sed '1,'"$NodesPerDC"'d' tmpnodelist > tmp
    cat tmp > tmpnodelist
    
    BenchNodeArray[$DCNum]=`head -$BenchNodesPerDC tmpnodelistbench`
    sed '1,'"$BenchNodesPerDC"'d' tmpnodelistbench > tmp
    cat tmp > tmpnodelistbench
done

# Run the benchmarks in parallel
# This is not a good way to do this, should be implemented inside basho bench
for DCNum in $(seq 1 $NumberDC); do
    TmpArray=(${BenchNodeArray[$DCNum]})
    for Item in ${TmpArray[@]}; do
	for I in $(seq 1 $BenchParallel); do
	    echo Running bench $I on $Item with nodes "${NodeArray[$DCNum]}"
	    echo "${NodeArray[$DCNum]}" > ./tmp
	    scp -o StrictHostKeyChecking=no -i key ./tmp root@"$Item":/root/basho_bench"$I"/basho_bench/script/runnodes
    	    ssh -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmark.sh $BenchmarkType $I &
	done
    done
done
wait




#./script/runSimpleBenchmark.sh $4 $BenchmarkType
