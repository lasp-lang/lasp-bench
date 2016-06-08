#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: numdcs nodesperdc parallelbench numbenchnodesperdc jobid time benchfile"
	exit
fi

NumDCs=$1
NodesPerDC=$2
BenchParallel=$3
NumBenchNodes=$4
GridJob=$5
Time=$6
BenchFile=$7

cd /root/basho_bench1/basho_bench/
mkdir -p logs/"$GridJob"

AllNodes=`cat script/allnodes`
AllCookies=`cat script/allcookies`
BenchNodes=`cat script/allnodesbench`
Branch=`cat script/branch`
echo All nodes for muliptle tests: $AllNodes
echo All cookies for muliptle tests: $AllCookies
echo Bench nodes for multiple tests: $BenchNodes
echo Branch using: $Branch
Mode="pb"
echo Starting time stabalize
./script/stablizeTime.sh >> logs/"$GridJob"/stabalize_time-"$Time" &
Pid=$!

#RingSize=$(($NodesPerDC * 12))

if [ $NodesPerDC -lt 2 ]
then
    RingSize=8
elif [ $NodesPerDC -lt 3 ]
then
    RingSize=128
elif [ $NodesPerDC -lt 5 ]
then
    RingSize=256
elif [ $NodesPerDC -lt 14 ]
then
    RingSize=256
elif [ $NodesPerDC -lt 33 ]
then
    RingSize=128
elif [ $NodesPerDC -lt 65 ]
then
    RingSize=256
else
    RingSize=512
fi

echo Setting partition size to $RingSize
./script/changePartition.sh $RingSize >> logs/"$GridJob"/changePartition-"$Time"

./script/runMultiDCBenchmark.sh antidote $NumDCs $NodesPerDC $NumBenchNodes 1 $BenchFile $BenchParallel $GridJob $Time

echo Stabalize time stopped
kill $Pid
