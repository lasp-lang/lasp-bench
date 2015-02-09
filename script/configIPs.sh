#!/bin/bash

AllNodes="172.31.54.217 172.31.54.218 172.31.54.219 172.31.54.220 172.31.54.221"
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
Command1="sudo sed -i 's/172.31.52.119/localhost/g' $File1"
Command2="sudo sed -i 's/127.0.0.1/localhost/g' $File2"
Command3="cd ./antidote/ && sudo git fetch && sudo git checkout evaluation && sudo make rel"
./script/command_to_all.sh "$AllNodes" "$Command1"	
./script/command_to_all.sh "$AllNodes" "$Command2"	
./script/command_to_all.sh "$AllNodes" "$Command3"	
