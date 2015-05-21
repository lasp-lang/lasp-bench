#!/bin/bash

Command1="cd ./basho_bench && sudo ./script/cleanupMymachine.sh"
Command2="sudo echo $2 | tr ' ' '\n' | sudo tee ./basho_bench/script/allnodes"
Command3="cd basho_bench && sudo ./script/addHost.sh"

#Clean up machine
./deployScript/parallel_command.sh "$1" "$Command1"

#Add number of nodes
./deployScript/command_to_all.sh "$1" "$Command2"

#Config antidote nodes
./deployScript/command_to_all.sh "$1" "$Command3"


