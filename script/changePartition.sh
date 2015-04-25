#!/bin/bash

Nodes=$1
File1="./antidote/rel/antidote/etc/app.config"
Command1="sudo sed '7d' $File1"
Command2="sudo sed -i '7i  {ring_creation_size, $2}' $File1"
Command3="sudo rm -r ./antidote/rel/antidote/data/*"

./script/parallel_command.sh "$Nodes" "$Command1"
./script/parallel_command.sh "$Nodes" "$Command2"
./script/parallel_command.sh "$Nodes" "$Command3"
