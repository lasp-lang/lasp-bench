#!/bin/bash

echo "Fetching latest update for basho_bench"

#Fetch latest update
git stash
git pull origin benchmark_cluster
rm -rf deps/riak_pb
rm -rf deps/antidote_pb
make

#Remake key
rm ./key
ln -s ../Antidote_bench.pem key

#echo "Fetching latest update for antidote"
#cd ../antidote
#sudo git stash
#sudo git pull origin evaluation
#sudo make


