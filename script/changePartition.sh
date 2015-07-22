#!/bin/bash

Nodes=`cat script/allnodes`
File1="./antidote/rel/antidote/etc/app.config"
Command1="sed -i '7d' $File1 &&  sed -i '7i \'$'\n' $File1"
Command2="sed -i '7i  {ring_creation_size, $1},' $File1"
Command3="rm -r ./antidote/rel/antidote/data/*"

./script/parallel_command.sh "$Nodes" "$Command1"
./script/parallel_command.sh "$Nodes" "$Command2"
./script/parallel_command.sh "$Nodes" "$Command3"
