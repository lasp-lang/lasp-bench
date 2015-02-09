#!/bin/bash

First=$1
Others=$2

Join="sudo antidote/rel/antidote/bin/antidote-admin cluster join antidote@$First"
Plan="sudo antidote/rel/antidote/bin/antidote-admin cluster plan"
Commit="sudo antidote/rel/antidote/bin/antidote-admin cluster commit"
./script/command_to_all.sh "$Others" "$Join"
./script/command_to_all.sh "$First" "$Plan"
./script/command_to_all.sh "$First" "$Commit"
