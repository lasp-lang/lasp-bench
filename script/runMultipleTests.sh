#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc"
	exit
fi

NumDCs=$1
NodesPerDC=$2
Id=$3

cd /root/basho_bench"$Id"/basho_bench/
AllNodes=`cat script/allnodes`
echo All nodes: $AllNodes
Mode="pb"
./script/stablizeTime.sh &
Pid=$!
./script/changePartition.sh $NodesPerDC
./script/runMultiDCBenchmark.sh "$AllNodes" antidote $NumDCs $NodesPerDC 1 $Mode 

tar cvzf /root/test"$Id".tar /root/basho_bench"$Id"/basho_bench/tests

kill $Pid
