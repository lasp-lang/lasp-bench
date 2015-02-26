#!/bin/bash

#AllNodes="172.31.54.217 172.31.54.218 172.31.54.219 172.31.54.220 172.31.54.221"
AllNodes="172.31.23.49 172.31.30.71"
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
Command1="sudo sed -i 's/172.31.30.71/localhost/g' $File1"
Command2="sudo sed -i 's/127.0.0.1/localhost/g' $File2"
#Command3="cd ./antidote/ && sudo git fetch && sudo git checkout evaluation && sudo make rel"
Command3="cd ./antidote/ && sudo make rel"
./script/command_to_all.sh "$AllNodes" "$Command1"	
./script/command_to_all.sh "$AllNodes" "$Command2"	
./script/command_to_all.sh "$AllNodes" "$Command3"	
