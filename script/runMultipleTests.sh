#!/bin/bash

#FirstNode="172.31.52.119"
#AllNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
#RestNodes="172.31.52.119 172.31.48.15 172.31.59.7 172.31.59.197 172.31.59.198"
FirstNode="172.31.52.16"
AllNodes="172.31.52.16 172.31.52.17 172.31.52.18 172.31.52.19 172.31.52.20" 
RestNodes="172.31.52.16 172.31.52.17 172.31.52.18 172.31.52.19 172.31.52.20" 
#AllNodes="172.31.52.119 10.20.0.78 10.20.0.79"
#RestNodes="172.31.52.119 10.20.0.78"
Cookie="antidote"
Mode=$1
./script/runMultiDCBenchmark.sh "$AllNodes" "$FirstNode" antidote 0 $Mode 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 0 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1 $Mode
