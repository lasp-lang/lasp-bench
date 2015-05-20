#!/bin/bash

NumDC=$1
NumNodesPerDC=$2
AllBenchmarkers=$3
FirstNodeExternalIPs=$4

#Change the number of partitions
Command1="sudo ./basho_bench/script/changePartition.sh $NumNodesPerDC"
Command2="sudo ./basho_bench/script/startNodes.sh"
#Join nodes to each cluster
Command3="sudo ./basho_bench/script/joinNodesToRing.sh $NumNodesPerDC"
Command4="sudo ./basho_bench/script/listenOnFirstNode.sh"
Command5="sudo ./basho_bench/script/connectNodesToOtherDCs.sh $NumNodesPerDC $FirstNodeExternalIPs"

./deployScript/parallel_command.sh "$3" $Command1
./deployScript/parallel_command.sh "$3" $Command2
./deployScript/parallel_command.sh "$3" $Command3
./deployScript/parallel_command.sh "$3" $Command4
./deployScript/parallel_command.sh "$3" $Command5

