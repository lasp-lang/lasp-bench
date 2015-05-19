#!/bin/bash

AllBenchmarkers=$1
NodesPerDC=$2
Command1="./basho_bench/script/runSimpleBenchmark.sh $NodesPerDC 1"
ResultToGet="./basho_bench/tests/current"
Folder="./results/"`date +"%H:%M:%S_%m_%d_%Y"`
mkdir $Folder

./script/parallel_command.sh "$AllBenchmarkers" "$Command1"
./script/parallel_fetch.sh $AllBenchmarkers $ResultToGet $Folder 
