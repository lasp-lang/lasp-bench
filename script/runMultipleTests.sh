#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc"
	exit
fi

NumDCs=$1
NodesPerDC=$2
AllNodes=`cat script/allnodes`
Mode="pb"
./script/stablizeTime.sh &
Pid=$!
./script/changePartitions.sh $NodesPerDC
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote $NumDCs $NodesPerDC 1 $Mode 

sudo kill $Pid
