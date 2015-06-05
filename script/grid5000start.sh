#!/bin/bash

#if [ $# -eq 0 ]; then
#    excho "Usage clusters"
#    exit
#fi

Clusters=(`oargridstat | awk '/-->/ { print $1 }'`)
Reservations=(`oargridstat | awk '/-->/ { print $3 }'`)
JobId=`oargridstat | awk '/Reservation/ { print $3 }' | grep -o '[0-9]*'`

for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    echo ${Clusters[$I]}
    ssh ${Clusters[$I]} ~/basho_scripts/grid5000start-createnodes.sh ${Clusters[$I]} $JobId &
    #oargridstat -w -l $JobId | sed '/^$/d' > ~/machines
    #awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines-tmp
    #kadeploy3 -f ~/machines-tmp -a ~/antidote_images/mywheezy-x64-base.env -k ~/.ssh/exp_key.pub
done
wait
