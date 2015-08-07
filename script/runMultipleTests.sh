#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc parallelbench numbenchnodesperdc"
	exit
fi

NumDCs=$1
NodesPerDC=$2
BenchParallel=$3
NumBenchNodes=$4
GridJob=$5
Time=$6

cd /root/basho_bench1/basho_bench/
mkdir -p logs/"$GridJob"

AllNodes=`cat script/allnodes`
BenchNodes=`cat script/allnodesbench`
echo All nodes for muliptle tests: $AllNodes
echo Bench nodes for multiple tests: $BenchNodes
Mode="pb"
./script/stablizeTime.sh >> logs/$GridJob/stabalize_time-$Time &
Pid=$!

#RingSize=$(($NodesPerDC * 12))
echo Setting partition size to $RingSize
RingSize=64
./script/changePartition.sh $RingSize >> logs/"$GridJob"/changePartition-"$Time"

./script/runMultiDCBenchmark.sh "$AllNodes" antidote $NumDCs $NodesPerDC $NumBenchNodes 1 $Mode $BenchParallel $GridJob $Time

#This is not right

kill $Pid
