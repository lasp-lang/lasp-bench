#!/bin/bash

if [ $# -eq 0 ]
then
    Branch="evaluation_pb_tx"
    echo "INFO: Using default parameter: Branch"$Branch
else
    Branch=$1
fi

AllNodes=`cat script/allnodes`
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
Command0="cd ./antidote/ && sudo ./rel/antidote/bin/antidote stop && sudo git stash && sudo git fetch && sudo git checkout $Branch && sudo git pull"
Command1="sudo sed -i 's/127.0.0.1/localhost/g' $File1"
Command2="sudo sed -i 's/172.31.30.71/localhost/g' $File1"
Command3="sudo sed -i 's/127.0.0.1/localhost/g' $File2"
Command4="cd ./antidote/ && sudo rm -r deps && sudo mkdir deps "
Command5="cd ./antidote/ && sudo make rel" 
./script/parallel_command.sh "$AllNodes" "$Command0"	
./script/parallel_command.sh "$AllNodes" "$Command1"	
./script/parallel_command.sh "$AllNodes" "$Command2"	
./script/parallel_command.sh "$AllNodes" "$Command3"	
./script/parallel_command.sh "$AllNodes" "$Command4"	
./script/parallel_command.sh "$AllNodes" "$Command5"	
