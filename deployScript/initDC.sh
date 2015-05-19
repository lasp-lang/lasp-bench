#!/bin/bash

Command1="sudo ./basho_bench/script/cleanupMymachine.sh"
Command2="cd basho_bench && sudo rm ./key && sudo ln -s ../Antidote_bench.pem key"
Command3="sudo echo $2 > ./basho_bench/script/allnodes"
Command4="cd basho_bench && sudo ./script/addHost.sh"

#Clean up machine
./deployScript/command_to_all.sh $1 $Command1 

#Regenerate key
./deployScript/command_to_all.sh $1 $Command2 

#Add number of nodes
./deployScript/command_to_all.sh $1 $Command3 

#Config antidote nodes
./deployScript/command_to_all.sh $1 $Command4 


