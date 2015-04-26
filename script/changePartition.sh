#!/bin/bash

Nodes=`cat script/allnodes`
File1="./antidote/rel/antidote/etc/app.config"
Command1="sudo sed -i '7d' $File1 &&  sudo sed -i '7i \'$'\n' $File"
Command2="sudo sed -i '7i  {ring_creation_size, $1},' $File1"
Command3="sudo rm -r ./antidote/rel/antidote/data/*"

./script/parallel_command.sh "$Nodes" "$Command1"
./script/parallel_command.sh "$Nodes" "$Command2"
./script/parallel_command.sh "$Nodes" "$Command3"
