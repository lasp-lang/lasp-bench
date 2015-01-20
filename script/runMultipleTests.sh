#!/bin/bash

AllNodes="10.20.0.27 10.20.0.78 10.20.0.79"
RestNodes="10.20.0.27 10.20.0.78"
Cookie="antidote"
./script/runMultiDCBenchmark.sh "$AllNodes" "10.20.0.27" antidote 0 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 0 
./script/runMultiDCBenchmark.sh "$AllNodes" "$RestNodes" antidote 1
