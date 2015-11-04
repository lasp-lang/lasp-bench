#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc, bench_nodes_per_dc, connect_dc_or_not, erl|pb, bench_parallel gridJob startTime"
    exit
else
    #AllSystemNodes=$1
    #SystemNodesArray=($AllSystemNodes)
    Cookie=$1
    NumberDC=$2
    NodesPerDC=$3
    BenchNodesPerDC=$4
    BenchNodes=`cat script/allnodesbench`
    NodesToUse=$((NumberDC * NodesPerDC))
    AllNodes=`cat script/allnodes`    
    # AllNodes=${SystemNodesArray[@]:0:$NodesToUse}
    # AllNodes=`echo ${AllNodes[@]}`
    ConnectDCs=$5
    echo "Using" $AllNodes ", will connect DCs:" $ConnectDCs
    BenchmarkFile=$6
    # if [ "$6" = "erl" ]; then
    # 	echo "Benchmark erl"
    #     BenchmarkType=0
    # elif [ "$6" = "pb" ]; then
    # 	echo "Benchmark pb"
    #     BenchmarkType=1
    # else
    #     echo "Wrong benchmark type!"
    #     exit
    # fi
    BenchParallel=$7
    GridJob=$8
    Time=$9
    Branch=`cat script/branch`
fi

BenchmarkType=1

ReadsNumber=( 10000 99 90 75 50 1 )
WritesNumber=( 1 1 10 25 50 100 )

ReadWrite=2

echo Stopping nodes
./script/stopNodes.sh  >> logs/"$GridJob"/stop_nodes-"$Time"

echo Deploying DCs
./script/deployMultiDCs.sh nodes $Cookie $ConnectDCs $NodesPerDC $Branch $BenchmarkFile
    
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
	    echo Running bench $I on $Item with nodes
	    echo "${NodeArray[$DCNum]}" > ./tmp
	    echo scp -o StrictHostKeyChecking=no -i key ./tmp root@"$Item":/root/basho_bench"$I"/basho_bench/script/runnodes
	    scp -o StrictHostKeyChecking=no -i key ./tmp root@"$Item":/root/basho_bench"$I"/basho_bench/script/runnodes
    	    echo ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/singleDemoBenchNode.sh $BenchmarkType $I $BenchmarkFile ${ReadsNumber[$ReadWrite]} ${WritesNumber[$ReadWrite]} $NumberDC $NodesPerDC $DCNum
	    
	    echo for job $GridJob on time $Time
    	    ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/singleDemoBenchNode.sh $BenchmarkType $I $BenchmarkFile ${ReadsNumber[$ReadWrite]} ${WritesNumber[$ReadWrite]}  $NumberDC $NodesPerDC $DCNum >> logs/"$GridJob"/runBench-"$Item"-"$I"-"$Time"-Reads"${ReadsNumber[$ReadWrite]}" 
	done
    done
done




#./script/runSimpleBenchmark.sh $4 $BenchmarkType
