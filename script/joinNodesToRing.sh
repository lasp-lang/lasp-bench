#!/bin/bash

First="10.20.0.77"
Others="10.20.0.78 10.20.0.79"
AllNodes="10.20.0.77 10.20.0.78 10.20.0.79"

Join="sudo antidote/rel/antidote/bin/antidote-admin cluster join antidote@$First"
Plan="sudo antidote/rel/antidote/bin/antidote-admin cluster plan"
Commit="sudo antidote/rel/antidote/bin/antidote-admin cluster commit"
./script/command_to_all.sh "$Others" "$Join"
./script/command_to_all.sh "$First" "$Plan"
./script/command_to_all.sh "$First" "$Commit"
