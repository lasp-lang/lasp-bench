#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc"
	exit
fi

cd /root/basho_bench/

NumDCs=$1
NodesPerDC=$2
AllNodes=`cat script/allnodes`
Mode="pb"
./script/stablizeTime.sh &
Pid=$!
./script/changePartition.sh $NodesPerDC
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote $NumDCs $NodesPerDC 1 $Mode 

tar cvzf /root/test.tar /root/basho_bench/tests

kill $Pid
