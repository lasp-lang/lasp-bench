#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc"
	exit
fi

NumDCs=$1
NodesPerDC=$2
BenchParallel=$3

cd /root/basho_bench1/basho_bench/
AllNodes=`cat script/allnodes`
BenchNodes=`cat script/allnodesbench`
echo All nodes: $AllNodes
echo Bench nodes $BenchNodes
Mode="pb"
./script/stablizeTime.sh &
Pid=$!
./script/changePartition.sh $NodesPerDC

./script/runMultiDCBenchmark.sh "$AllNodes" antidote $NumDCs $NodesPerDC 1 $Mode $BenchParallel

tar cvzf /root/test"$Id".tar /root/basho_bench"$Id"/basho_bench/tests

kill $Pid
