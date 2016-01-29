#!/bin/bash


if [ $# -eq 1 ]
then
    First=`head -1 ./script/allnodes`
    Others=`awk -v var="$1" 'NR>=2&&NR<=var' ./script/allnodes`
    echo Parameter:$1 , joinig $Others to $First
else
    First=$1
    Others=$2
    echo "Joinig "$Others " to "$First
fi

sleep 30

Join="antidote/rel/antidote/bin/antidote-admin cluster join antidote@$First"
Plan="antidote/rel/antidote/bin/antidote-admin cluster plan"
Commit="antidote/rel/antidote/bin/antidote-admin cluster commit"
./script/command_to_all_slow.sh "$Others" "$Join"

sleep 60

./script/command_to_all.sh "$First" "$Plan"
./script/command_to_all.sh "$First" "$Commit"

