#!/bin/bash

NumDC=$1
NumNodesPerDC=$2
FirstNodeExternalIPs=$4

#Change the number of partitions
Command0="cd ./basho_bench && sudo ./script/stopNodes.sh"
Command1="cd ./basho_bench && sudo ./script/changePartition.sh $NumNodesPerDC"
Command2="cd ./basho_bench && sudo ./script/startNodes.sh"
#Join nodes to each cluster
Command3="cd ./basho_bench && sudo ./script/joinNodesToRing.sh $NumNodesPerDC"
Command4="cd ./basho_bench && sudo ./script/waitRingsToFinish.sh"
Command5="cd ./basho_bench && sudo ./script/listenOnFirstNode.sh"
Command6="cd ./basho_bench && sudo ./script/connectNodesToOtherDCs.sh $NumNodesPerDC \"$FirstNodeExternalIPs\""

./deployScript/parallel_command.sh "$3" "$Command0"
./deployScript/parallel_command.sh "$3" "$Command1"
./deployScript/parallel_command.sh "$3" "$Command2"
./deployScript/parallel_command.sh "$3" "$Command3"
./deployScript/parallel_command.sh "$3" "$Command4"
./deployScript/parallel_command.sh "$3" "$Command5"
./deployScript/parallel_command.sh "$3" "$Command6"

