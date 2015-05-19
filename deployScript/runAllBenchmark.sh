#!/bin/bash


DC1Benchmark=""
DC1Antidote=""

Branch1="evaluation"

AllMachines
AllBenchmarker

./deployScript/initDCs.sh $DC1Benchmark $DC1Antidote 

./deployScript/makeBranch.sh $AllBenchmarkers $Branch1
./deployScript/setupDCTopology.sh 1 3 $AllNodes
./deployScript/runBenchmark.sh $AllBenchmarkers 3

./deployScript/makeBranch.sh $AllBenchmarkers $Branch1
./deployScript/setupDCTopology.sh 1 3 $AllNodes
./deployScript/runBenchmark.sh $AllBenchmarkers 3



