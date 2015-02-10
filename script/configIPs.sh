#!/bin/bash

AllNodes="172.31.49.211 172.31.49.212 172.31.49.213 172.31.49.214"
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
Command1="sudo sed -i 's/172.31.52.119/localhost/g' $File1"
Command2="sudo sed -i 's/127.0.0.1/localhost/g' $File2"
Command3="cd ./antidote/ && sudo git fetch && sudo git checkout evaluation && sudo make rel"
./script/command_to_all.sh "$AllNodes" "$Command1"	
./script/command_to_all.sh "$AllNodes" "$Command2"	
./script/command_to_all.sh "$AllNodes" "$Command3"	
