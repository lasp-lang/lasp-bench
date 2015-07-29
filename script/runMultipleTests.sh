#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc parallelbench numbenchnodesperdc"
	exit
fi

NumDCs=$1
NodesPerDC=$2
BenchParallel=$3
NumBenchNodes=$4

cd /root/basho_bench1/basho_bench/
AllNodes=`cat script/allnodes`
BenchNodes=`cat script/allnodesbench`
echo All nodes: $AllNodes
echo Bench nodes $BenchNodes
Mode="pb"
./script/stablizeTime.sh &
Pid=$!

RingSize=$(($NodesPerDC * 32))
./script/changePartition.sh $RingSize

./script/runMultiDCBenchmark.sh "$AllNodes" antidote $NumDCs $NodesPerDC $NumBenchNodes 1 $Mode $BenchParallel

#This is not right
tar cvzf /root/test"$Id".tar /root/basho_bench"$Id"/basho_bench/tests

kill $Pid
