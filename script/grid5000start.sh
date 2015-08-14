#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage gridjobid branch dodeploy secondrun benchCount benchParallel benchFile"
    exit
fi

# Wait some time to be sure the reservations have started
# sleep 60s

GridJob=$1
Branch=$2
DoDeploy=$3
SecondRun=$4
BenchCount=$5
BenchParallel=$6
BenchFile=$7
Clusters=(`oargridstat $1 | awk '/-->/ { print $1 }'`)
# Reservations=(`oargridstat $1 | awk '/-->/ { print $3 }'`)
# JobId=`oargridstat $1 | awk '/Reservation/ { print $3 }' | grep -o '[0-9]*'`


# Filter out the names of the benchmark nodes and the computation nodes
rm ~/nodelist
rm ~/benchnodelist
for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    echo ${Clusters[$I]}
    oargridstat -w -l $GridJob | sed '/^$/d' > ~/machines
    awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines-tmp
    awk < ~/machines-tmp '!seen[$0]++' > ~/machines-tmp2
    head -"$BenchCount" ~/machines-tmp2 >> ~/benchnodelist
    sed '1,'"$BenchCount"'d' ~/machines-tmp2 >> ~/nodelist
done

echo Benchmark nodes: `cat ~/benchnodelist`
echo
echo Compute nodes: `cat ~/nodelist`
echo

if [ $DoDeploy -eq 1 ]; then
    # Connect to each cluster to deloy the nodes
    for I in $(seq 0 $((${#Clusters[*]} - 1))); do
	echo Deploying cluster: ${Clusters[$I]}
	ssh -t -o StrictHostKeyChecking=no ${Clusters[$I]} ~/basho_bench/script/grid5000start-createnodes.sh ${Clusters[$I]} $GridJob &
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
BenchNode=`head -1 ~/benchnodelist`

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
    AllNodes=`cat ~/benchnodelist`
    for I in $(seq 1 $BenchParallel); do
	echo Checking out 
	Command0="cd ./basho_bench"$I"/basho_bench/ && git stash && git fetch && git checkout grid5000 && git pull"
	~/basho_bench/script/parallel_command.sh "$AllNodes" "$Command0" >> logs/"$GridJob"/basho_bench-compile-job"$Time"
    done

    # Copy the allnodes file to the benchmark locations
    echo all nodes are `cat ~/nodelistip`
    echo Performing SCPs
    for Node in `cat ~/benchnodelist`; do
	for I in $(seq 1 $BenchParallel); do
	    echo scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    echo scp ~/benchnodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodesbench
	    scp ~/benchnodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodesbench
	done
    done

    echo Performins configMachines.sh on "$Branch"
    echo ssh root@$BenchNode /root/basho_bench1/basho_bench/script/configMachines.sh $Branch
    ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/configMachines.sh $Branch $GridJob $Time
    
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
echo Running the test at $BenchNode
echo ssh -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/runMultipleTests.sh $TotalDCs $Size $BenchParallel $BenchCount $GridJob $Time $BenchFile
ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/runMultipleTests.sh $TotalDCs $Size $BenchParallel $BenchCount $GridJob $Time $BenchFile

# Get the results

Reads=( 10000 99 90 75 50 1 )

echo Compiling the results
cd ~

mkdir antidote_bench-"$Time"
touch ~/antidote_bench-"$Time"/filenames

for ReadWrite in $(seq 0 5); do
#tar cvzf ./test.tar tests-$FileName-$Reads

    rm ~/antidote_bench-"$Time"/filenames
    for Node in `cat ~/benchnodelist`; do
	for I in $(seq 1 $BenchParallel); do
 	    echo scp -o StrictHostKeyChecking=no root@$Node:/root/basho_bench"$I"/basho_bench/test-"$BenchFile"-"${Reads[$ReadWrite]}".tar ~/antidote_bench-"$Time"/test"$Node"-"$I"-"$BenchFile"-"${Reads[$ReadWrite]}".tar
 	    scp -o StrictHostKeyChecking=no root@$Node:/root/basho_bench"$I"/basho_bench/test-"$BenchFile"-"${Reads[$ReadWrite]}".tar ~/antidote_bench-"$Time"/test"$Node"-"$I"-"$BenchFile"-"${Reads[$ReadWrite]}".tar
	    echo test"$Node"-"$I"-"$BenchFile"-"${Reads[$ReadWrite]}" >> ~/antidote_bench-"$Time"/filenames
	    echo test"$Node"-"$I"-"$BenchFile"-"${Reads[$ReadWrite]}" >> ~/antidote_bench-"$Time"/allfilenames
	done
    done

    echo Merging the results
    ./basho_bench/script/mergeResults.sh ~/antidote_bench-"$Time"/ "$BenchFile"-"${Reads[$ReadWrite]}"
    
done



echo Taring them to antidote_bench-"$Time".tar
tar cvzf antidote_bench-"$Time".tar antidote_bench-"$Time" >> logs/"$GridJob"/tar_merged_job"$Time"
