#!/bin/bash

#FirstNode="172.31.52.119"
#AllNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
#RestNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
FirstNode="172.31.30.219"
TwoNode="172.31.30.219 172.31.30.220"
FourNode="172.31.30.219 172.31.30.220 172.31.30.221 172.31.30.222"
SixNode="172.31.30.219 172.31.30.220 172.31.30.221 172.31.30.222 172.31.30.223 172.31.30.224"
AllNodes="172.31.30.219 172.31.30.220 172.31.30.221 172.31.30.222 172.31.30.223 172.31.30.224 172.31.30.225 172.31.30.226"
#AllNodes="172.31.52.119 10.20.0.78 10.20.0.79"
#RestNodes="172.31.52.119 10.20.0.78"
Cookie="antidote"
#Mode="erl"
#./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 1
#./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 2 
#./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 3 
#/script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 4 


Mode="pb"
#./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 1
./script/runMultiDCBenchmark.sh "$AllNodes" "$FirstNode" antidote 1 $Mode 1 
./script/runMultiDCBenchmark.sh "$AllNodes" "$TwoNode" antidote 1 $Mode 2 
./script/runMultiDCBenchmark.sh "$AllNodes" "$FourNode" antidote 1 $Mode 4 
./script/runMultiDCBenchmark.sh "$AllNodes" "$AllNodes" antidote 1 $Mode 8 

./script/runMultiDCBenchmark.sh "$AllNodes" "$TwoNode" antidote 2 $Mode 1
./script/runMultiDCBenchmark.sh "$AllNodes" "$FourNode" antidote 2 $Mode 2 
./script/runMultiDCBenchmark.sh "$AllNodes" "$AllNodes" antidote 2 $Mode 4 

./script/runMultiDCBenchmark.sh "$AllNodes" "$TwoNode" antidote 2 $Mode 1 
./script/runMultiDCBenchmark.sh "$AllNodes" "$FourNode" antidote 4 $Mode 1 
./script/runMultiDCBenchmark.sh "$AllNodes" "$AllNodes" antidote 8 $Mode 1 
#./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 3 
#./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode 4 
