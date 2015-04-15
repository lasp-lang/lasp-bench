#!/bin/bash

Nodes=$1
File1="./antidote/rel/antidote/etc/app.config"
Command1="sudo sed -i 's/ring_creation_size, .*}/ring_creation_size, $2}/g' $File1"
Command2="sudo rm -r ./antidote/rel/antidote/data/*"

./script/parallel_command.sh "$Nodes" "$Command1"
./script/parallel_command.sh "$Nodes" "$Command2"
