#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage gridjobid branch dodeploy secondrun computeCount benchCount benchParallel benchFile"
    exit
fi

# Wait some time to be sure the reservations have started
# sleep 60s

JobFile=$1
Branch=$2
DoDeploy=$3
SecondRun=$4
ComputeCount=$5
BenchCount=$6
BenchParallel=$7
BenchFile=$8
# Clusters=(`oargridstat $1 | awk '/-->/ { print $1 }'`)
# Reservations=(`oargridstat $1 | awk '/-->/ { print $3 }'`)
# JobId=`oargridstat $1 | awk '/Reservation/ { print $3 }' | grep -o '[0-9]*'`

Clusters=(`cat clusters`)
Jobs=(`cat jobs`)

GridJob=${Jobs[0]}

# Filter out the names of the benchmark nodes and the computation nodes
rm ~/nodelist
rm ~/benchnodelist
rm ~/fullnodelist
rm ~/machines
for I in $(seq 0 $((${#Jobs[*]} - 1))); do
    # oarstat -j "${Jobs[$I]}" -p | oarprint core -P host -f - >> machines
    ssh -o StrictHostKeyChecking=no ${Clusters[$I]} ~/basho_bench/script/get_machines.sh ${Clusters[$I]} ${Jobs[$I]} >> machines
    # awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines
done

CountDC=0
for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    echo ${Clusters[$I]}
    # oargridstat -w -l $GridJob | sed '/^$/d' > ~/machines
    awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines-tmp
    awk < ~/machines-tmp '!seen[$0]++' > ~/machines-tmp2
    awk < ~/machines-tmp '!seen[$0]++' >> ~/fullnodelist
    head -"$BenchCount" ~/machines-tmp2 >> ~/benchnodelist
    sed '1,'"$BenchCount"'d' ~/machines-tmp2 | head -"$ComputeCount" >> ~/nodelist
    CountDC=$(($CountDC + 1))
done

echo $CountDC > ~/countDC
echo $Branch > ~/branch

echo Benchmark nodes: `cat ~/benchnodelist`
echo
echo Compute nodes: `cat ~/nodelist`
echo
echo Full node list: `cat ~/fullnodelist`
echo
echo Branch to send: `cat ~/branch`
echo

if [ $DoDeploy -eq 1 ]; then
    # Connect to each cluster to deloy the nodes
    for I in $(seq 0 $((${#Clusters[*]} - 1))); do
	echo Deploying cluster: ${Clusters[$I]}
	ssh -t -o StrictHostKeyChecking=no ${Clusters[$I]} ~/basho_bench/script/grid5000start-createnodestemp.sh ${Clusters[$I]} ${Jobs[$I]} &
	#oargridstat -w -l $JobId | sed '/^$/d' > ~/machines
	#awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines-tmp
	#kadeploy3 -f ~/machines-tmp -a ~/antidote_images/mywheezy-x64-base.env -k ~/.ssh/exp_key.pub
    done
    wait
fi

# oargridstat -w -l $GridJob | sed '/^$/d' > ~/machines
# awk < ~/machines '!seen[$0]++' > ~/machines-tmp
# awk < ~/machines-tmp '!/'"$BenchNode"'/ { print $1 }' > ~/machines-tmp2

# Change node names to ips
while read in; do dig +short "$in"; done < ~/nodelist > ~/nodelistip
while read in; do dig +short "$in"; done < ~/benchnodelist > ~/benchnodelistip
while read in; do dig +short "$in"; done < ~/fullnodelist > ~/fullnodelistip
BenchNode=`head -1 ~/benchnodelist`

echo $BenchNode > tmpTime

# Calculate the number of DCs in case there is one that is just benchmark nodes
# Otherwise all DCs should have the same number of nodes
# (Should make this configurable in the future"
TotalDCs=0
for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    # echo ${Clusters[$I]}
    DCSize=`grep -o ${Clusters[$I]} ~/nodelist | wc -l`
    if [ $DCSize -ne 0 ]; then
	Size=$DCSize
	TotalDCs=$(($TotalDCs + 1))
    fi
done
echo Nodes per DC: $Size
echo Number of DCs: $TotalDCs

Time=`date +"%Y-%m-%d-%s"`
mkdir -p logs/"$GridJob"

if [ $SecondRun -eq 0 ]; then
    # The first run should download and update all code files
    echo The first run
    # AllNodes=`cat ~/benchnodelist`
    # Will compile both antidoe and basho bench on all nodes in case the number changes in a later experiment
    AllNodes=`cat ~/fullnodelist`

    echo Perform configProxy.sh on "$BenchNode"
    echo First copying the node list to "$BenchNode"
    echo scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodes
    scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodes
    echo scp ~/basho_bench/script/configProxy.sh root@"$BenchNode":/root/basho_bench1/basho_bench/script/
    scp ~/basho_bench/script/configProxy.sh root@"$BenchNode":/root/basho_bench1/basho_bench/script/
    echo ssh root@$BenchNode /root/basho_bench1/basho_bench/script/configProxy.sh
    ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/configProxy.sh


    for I in $(seq 1 $BenchParallel); do
	echo Checking out 
	Command0="cd ./basho_bench"$I"/basho_bench/ && rm -f ./script/configProxy.sh && git stash && git fetch && git checkout grid5000 && git pull && rm -rf ./deps/* && make all"
	~/basho_bench/script/parallel_command.sh "$AllNodes" "$Command0" >> logs/"$GridJob"/basho_bench-compile-job"$Time"
    done

    echo Performins configMachines.sh on "$BenchNode"
    echo First copying the node list to "$BenchNode"
    echo scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodes
    scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodes
    echo ssh root@$BenchNode /root/basho_bench1/basho_bench/script/configMachines.sh $Branch
    ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/configMachines.sh $Branch $GridJob $Time

    # Copy the allnodes file to the benchmark locations
    echo all nodes are `cat ~/nodelistip`
    echo Performing SCPs
    for Node in `cat ~/benchnodelist`; do
	for I in $(seq 1 $BenchParallel); do
	    echo scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    echo scp ~/benchnodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodesbench
	    scp ~/benchnodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodesbench
	    echo scp ~/branch root@"$Node":/root/basho_bench"$I"/basho_bench/script/branch
	    scp ~/branch root@"$Node":/root/basho_bench"$I"/basho_bench/script/branch
	done
    done
    
else
    # Copy the allnodes file to the benchmark locations
    echo Not the first run
    echo all nodes are `cat ~/nodelistip`
    for Node in `cat ~/benchnodelist`; do
	for I in $(seq 1 $BenchParallel); do
	    echo scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    echo scp ~/benchnodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodesbench
	    scp ~/benchnodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodesbench
	    echo scp ~/branch root@"$Node":/root/basho_bench"$I"/basho_bench/script/branch
	    scp ~/branch root@"$Node":/root/basho_bench"$I"/basho_bench/script/branch
	done
    done
   
fi

# The second run only need to do a make clean
AllNodes1=`cat ~/nodelist`
echo Performing a relclean
Command1="cd ./antidote/ && make relclean"
~/basho_bench/script/parallel_command.sh "$AllNodes1" "$Command1" >> logs/"$GridJob"/make_rel-job"$Time"

# Compile the code
echo Performing make again in case the first time there was an error
ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/makeRel.sh >> logs/"$GridJob"/make_rel-job"$Time"

# Run the benchmark
echo Starting the cluster at $BenchNode
echo ssh -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/setupClusterDemo.sh $TotalDCs $Size $BenchParallel $BenchCount $GridJob $Time $BenchFile
ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/setupClusterDemo.sh $TotalDCs $Size $BenchParallel $BenchCount $GridJob $Time $BenchFile
