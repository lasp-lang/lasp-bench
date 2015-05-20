#!/bin/bash

AllBenchmarkers=$1
NodesPerDC=$2
Command1="cd ./basho_bench && sudo ./script/runSimpleBenchmark.sh $NodesPerDC 1"
ResultToGet="./basho_bench/tests/current"
Folder="./results/"`date +"%Y-%m-%d-%H:%M:%S"`
mkdir $Folder

./script/parallel_command.sh "$AllBenchmarkers" "$Command1"
./script/parallel_fetch.sh "$AllBenchmarkers" $ResultToGet $Folder 
