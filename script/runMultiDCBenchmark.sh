#!/bin/bash

AllNodes="10.20.0.27 10.20.0.78 10.20.0.79"
Cookie="antidote"
./script/deployMultiDCs.sh "$Cookie" "$AllNodes"
sudo ./basho_bench examples/antidote.config
