#!/bin/bash

#FirstNode="172.31.52.119"
#AllNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
#RestNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
FirstNode="172.31.61.14"
AllNodes="172.31.61.14 172.31.61.15 172.31.61.16 172.31.61.17 172.31.61.18 172.31.61.19"
RestNodes="172.31.61.14 172.31.61.15 172.31.61.16 172.31.61.17 172.31.61.18 172.31.61.19"
#AllNodes="172.31.52.119 10.20.0.78 10.20.0.79"
#RestNodes="172.31.52.119 10.20.0.78"
Cookie="antidote"
Mode="pb"
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 1
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 2 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 3 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 6 


Mode="erl"
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 1
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 2 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 3 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 6 
