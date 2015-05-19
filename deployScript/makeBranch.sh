#!/bin/bash

Command1="sudo ./basho_bench/script/configMachines.sh $2"
Command2="sudo ./basho_bench/script/makeRel.sh"

./script/parallel_command.sh "$1" $Command1
./script/parallel_command.sh "$1" $Command2
