#!/bin/bash

DC1Benchmark="52.6.142.114"
DC1Antidote="172.31.30.39 172.31.30.40"

DC2Benchmark="54.77.86.35"
DC2Antidote="172.31.31.74 172.31.31.75"

AllBenchmarkers="52.6.142.114  54.77.86.35"
AllNodes="52.1.88.171 52.7.209.129 54.77.63.61 52.17.155.188"

Branch1="evaluation"


./deployScript/initDC.sh $DC1Benchmark "$DC1Antidote"
./deployScript/initDC.sh $DC2Benchmark "$DC2Antidote" 

./deployScript/makeBranch.sh "$AllBenchmarkers" "$Branch1"
./deployScript/setupDCTopology.sh 2 2 "$AllBenchmarkers" "52.1.88.171 54.77.63.61"
./deployScript/runBenchmark.sh $AllBenchmarkers 2 

#./deployScript/makeBranch.sh $AllBenchmarkers $Branch1
#./deployScript/setupDCTopology.sh 1 3 $AllNodes
#./deployScript/runBenchmark.sh $AllBenchmarkers 3



