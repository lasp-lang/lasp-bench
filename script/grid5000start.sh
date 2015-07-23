#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage gridjobid benchnode branch dodeploy"
    exit
fi

# Wait some time to be sure the reservations have started
# sleep 60s

GridJob=$1
BenchNode=$2
Branch=$3
DoDeploy=$4
Clusters=(`oargridstat $1 | awk '/-->/ { print $1 }'`)
# Reservations=(`oargridstat $1 | awk '/-->/ { print $3 }'`)
# JobId=`oargridstat $1 | awk '/Reservation/ { print $3 }' | grep -o '[0-9]*'`

if [ $DoDeploy -eq 1 ]; then
    
    for I in $(seq 0 $((${#Clusters[*]} - 1))); do
	echo ${Clusters[$I]}
	ssh ${Clusters[$I]} ~/basho_bench/script/grid5000start-createnodes.sh ${Clusters[$I]} $GridJob &
	#oargridstat -w -l $JobId | sed '/^$/d' > ~/machines
	#awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines-tmp
	#kadeploy3 -f ~/machines-tmp -a ~/antidote_images/mywheezy-x64-base.env -k ~/.ssh/exp_key.pub
    done
    wait
fi

oargridstat -w -l $GridJob | sed '/^$/d' > ~/machines
awk < ~/machines '!/'"$BenchNode"'/ { print $1 }' > ~/machines-tmp
awk < ~/machines-tmp '!seen[$0]++' > ~/machines-tmp2
while read in; do dig +short "$in"; done < ~/machines-tmp2 > ~/allnodes

TotalDCs=0
for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    echo ${Clusters[$I]}
    DCSize=`grep -o ${Clusters[$I]} machines-tmp2 | wc -l`
    if [ $DCSize -ne 0 ]; then
	Size=$DCSize
	TotalDCs=$(($TotalDCs + 1))
    fi
done
echo Nodes per DC: $Size
echo Number of DCs: $TotalDCs

AllNodes=`cat ~/machines-tmp2`
Command0="cd ./basho_bench/ && git stash && git fetch && git checkout grid5000 && git pull"
~/basho_bench/script/parallel_command.sh "$AllNodes" "$Command0"	

scp ~/allnodes root@$BenchNode:~/basho_bench/script/allnodes
ssh root@$BenchNode /root/basho_bench/script/configMachines.sh $Branch
ssh root@$BenchNode /root/basho_bench/script/makeRel.sh
ssh root@$BenchNode /root/basho_bench/script/runMultipleTests.sh $TotalDCs $Size
scp root@$BenchNode:/root/test.tar ~/
