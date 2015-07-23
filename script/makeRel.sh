#!/bin/bash

cd /root/basho_bench/

AllNodes=`cat script/allnodes`
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
Command1="./antidote/rel/antidote/bin/antidote stop" 
Command3="cd ./antidote/ && make rel"
cd /root/basho_bench/
./script/parallel_command.sh "$AllNodes" "$Command1"	
./script/parallel_command.sh "$AllNodes" "$Command3"	
