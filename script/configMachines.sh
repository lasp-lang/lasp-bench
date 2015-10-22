#!/bin/bash

if [ $# -eq 0 ]
then
    Branch="evaluation_pb_tx"
    echo "INFO: Using default parameter: Branch"$Branch
else
    Branch=$1
    JobId=$2
    Time=$3
fi

cd /root/basho_bench1/basho_bench/

AllNodes=`cat /root/basho_bench1/basho_bench/script/allnodes`

echo All nodes "$AllNodes"
mkdir -p logs/"$JobId"

File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
Command0="cd ./antidote/ && ./rel/antidote/bin/antidote stop"
Command0a="cd ./antidote/ && git stash && git fetch && git pull && git checkout $Branch && git pull"
Command0b="cd ./antidote/ && git checkout $Branch && git pull"
Command1="sed -i 's/127.0.0.1/localhost/g' $File1"
Command2="sed -i 's/172.31.30.71/localhost/g' $File1"
Command3="sed -i 's/127.0.0.1/localhost/g' $File2"
Command4="cd ./antidote/ && rm -r deps && mkdir deps "
Command5="cd ./antidote/ && make relnocert" 

echo Running config commands
echo

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command0"	
./script/parallel_command.sh "$AllNodes" "$Command0" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command0a"	
./script/parallel_command.sh "$AllNodes" "$Command0a" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command0b"	
./script/parallel_command.sh "$AllNodes" "$Command0b" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command1"	
./script/parallel_command.sh "$AllNodes" "$Command1" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command2"	
./script/parallel_command.sh "$AllNodes" "$Command2" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command3"	
./script/parallel_command.sh "$AllNodes" "$Command3" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command4"	
./script/parallel_command.sh "$AllNodes" "$Command4" >> logs/"$JobId"/config_machines-"$Time"

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command5"	
./script/parallel_command.sh "$AllNodes" "$Command5" >> logs/"$JobId"/config_machines-"$Time"
echo
