#!/bin/bash

AllNodes=`cat script/allnodes`
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
#Command0="cd ./antidote/ && sudo git stash && sudo git fetch && sudo git checkout opt_evaluation && sudo git pull"
Command0="cd ./antidote/ && sudo ./rel/antidote/bin/antidote stop && sudo git stash && sudo git fetch && sudo git checkout evaluation && sudo git pull"
Command1="sudo sed -i 's/127.0.0.1/localhost/g' $File1"
Command1b="sudo sed -i 's/172.31.30.71/localhost/g' $File1"
Command2="sudo sed -i 's/127.0.0.1/localhost/g' $File2"
Command3="cd ./antidote/ && sudo make rel"
Command4="sudo ntpdate ntp.ubuntu.com"
./script/command_to_all.sh "$AllNodes" "$Command0"	
./script/command_to_all.sh "$AllNodes" "$Command1"	
./script/command_to_all.sh "$AllNodes" "$Command1b"	
./script/command_to_all.sh "$AllNodes" "$Command2"	
./script/command_to_all.sh "$AllNodes" "$Command3"	
./script/command_to_all.sh "$AllNodes" "$Command4"	
