#!/bin/bash

#FirstNode="172.31.52.119"
#AllNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
#RestNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
FirstNode="172.31.54.217"
AllNodes="172.31.54.217 172.31.54.218 172.31.54.219 172.31.54.220 172.31.54.221"
RestNodes="172.31.54.217 172.31.54.218 172.31.54.219 172.31.54.220 172.31.54.221"
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
