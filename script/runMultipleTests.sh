#!/bin/bash

#FirstNode="172.31.52.119"
#AllNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
#RestNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
FirstNode="172.31.52.203"
AllNodes="172.31.52.203 172.31.52.204 172.31.52.205 172.31.52.206 172.31.52.207"
RestNodes="172.31.52.203 172.31.52.204 172.31.52.205 172.31.52.206 172.31.52.207"
#AllNodes="172.31.52.119 10.20.0.78 10.20.0.79"
#RestNodes="172.31.52.119 10.20.0.78"
Cookie="antidote"
Mode="pb"
./script/runMultiDCBenchmark.sh "$AllNodes" "$FirstNode" antidote 0 $Mode 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 0 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode


Mode="erl"
./script/runMultiDCBenchmark.sh "$AllNodes" "$FirstNode" antidote 0 $Mode 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 0 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode
