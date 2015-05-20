#!/bin/bash

Command1="cd ./basho_bench && sudo ./script/configMachines.sh $2"
Command2="cd ./basho_bench && sudo ./script/makeRel.sh"

./script/parallel_command.sh "$1" "$Command1"
./script/parallel_command.sh "$1" "$Command2"
