#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage configlefile whit lines gridjobid branch dodeploy secondrun computeCount benchCount benchParallel dcspercluster benchBranch benchFile"
    exit
fi

# Wait some time to be sure the reservations have started
# sleep 60s

IFS=$'\r\n' GLOBIGNORE='*' :; Config=($(cat $1))

GridJob=${Config[0]}
Branch=${Config[1]}
DoDeploy=${Config[2]}
SecondRun=${Config[3]}
ComputeCount=${Config[4]}
BenchCount=${Config[5]}
BenchParallel=${Config[6]}
DcsPerCluster=${Config[7]}
GridBranch=${Config[8]}
BenchFile=${Config[9]}
Clusters=(`oargridstat $GridJob | awk '/-->/ { print $1 }'`)
# Reservations=(`oargridstat $1 | awk '/-->/ { print $3 }'`)
# JobId=`oargridstat $1 | awk '/Reservation/ { print $3 }' | grep -o '[0-9]*'`

BenchCount2=$(($BenchCount * $DcsPerCluster))
ComputeCount2=$(($ComputeCount * $DcsPerCluster))

# Filter out the names of the benchmark nodes and the computation nodes
rm ~/nodelist
rm ~/benchnodelist
rm ~/fullnodelist
CountDC=0
for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    echo ${Clusters[$I]}
    oargridstat -w -l $GridJob | sed '/^$/d' > ~/machines
    awk < ~/machines '/'"${Clusters[$I]}"'/ { print $1 }' > ~/machines-tmp
    awk < ~/machines-tmp '!seen[$0]++' > ~/machines-tmp2
    awk < ~/machines-tmp '!seen[$0]++' >> ~/fullnodelist
    head -"$BenchCount2" ~/machines-tmp2 >> ~/benchnodelist
    sed '1,'"$BenchCount2"'d' ~/machines-tmp2 | head -"$ComputeCount2" >> ~/nodelist
    CountDC=$(($CountDC + 1))
done

echo $CountDC > ~/countDC
echo $Branch > ~/branch

cat ~/benchnodelist ~/nodelist > ~/fullnodelist

echo Benchmark nodes: `cat ~/benchnodelist`
echo
echo Compute nodes: `cat ~/nodelist`
echo
echo Full node list: `cat ~/fullnodelist`
echo
echo Branch to send: `cat ~/branch`
echo

# Change node names to ips
while read in; do dig +short "$in"; done < ~/nodelist > ~/nodelistip
while read in; do dig +short "$in"; done < ~/benchnodelist > ~/benchnodelistip
while read in; do dig +short "$in"; done < ~/fullnodelist > ~/fullnodelistip
BenchNode=`head -1 ~/benchnodelist`

# Calculate the number of DCs in case there is one that is just benchmark nodes
# Otherwise all DCs should have the same number of nodes
# (Should make this configurable in the future"
TotalDCs=0
for I in $(seq 0 $((${#Clusters[*]} - 1))); do
    # echo ${Clusters[$I]}
    DCSize=`grep -o ${Clusters[$I]} ~/nodelist | wc -l`
    if [ $DCSize -ne 0 ]; then
	Size=$(($DCSize / $DcsPerCluster))
	TotalDCs=$(($TotalDCs + 1))
    fi
done
TotalDCs=$(($TotalDCs * $DcsPerCluster))
echo Nodes per DC: $Size
echo Number of DCs: $TotalDCs


echo Benchmark nodes: `cat ~/benchnodelist`
echo
echo Compute nodes: `cat ~/nodelist`
echo
echo Full node list: `cat ~/fullnodelist`


rm ~/benchcookielist
rm ~/computecookielist
rm ~/allcookielist
echo Making cookies

DCNum=1
for I in $(seq 1 $CountDC); do
    for F in $(seq 1 $DcsPerCluster); do
	for J in $(seq 1 $BenchCount); do
	    echo dccookie"$DCNum" >> ~/allcookielist
	done
	DCNum=$(($DCNum + 1))
    done
done
DCNum=1
for I in $(seq 1 $CountDC); do
    for F in $(seq 1 $DcsPerCluster); do
	for J in $(seq 1 $ComputeCount); do
	    echo dccookie"$DCNum" >> ~/allcookielist
	done
	DCNum=$(($DCNum + 1))
    done
done

DCNum=1
for I in $(seq 1 $TotalDCs); do
    for F in $(seq 1 $BenchCount); do
	echo dccookie"$DCNum" >> ~/benchcookielist	
    done
    for F in $(seq 1 $ComputeCount); do
	echo dccookie"$DCNum" >> ~/computecookielist
    done
    DCNum=$(($DCNum + 1))
done
echo Benchmark cookies: `cat ~/benchcookielist`
echo Compute cookies: `cat ~/computecookielist`
echo All cookies: `cat ~/allcookielist`	    



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

Time=`date +"%Y-%m-%d-%s"`
mkdir -p logs/"$GridJob"

echo Copying the experiment key to "$BenchNode"
echo scp ~/key root@"$BenchNode":/root/basho_bench1/basho_bench/
scp ~/key root@"$BenchNode":/root/basho_bench1/basho_bench/


if [ $SecondRun -eq 0 ]; then
    # The first run should download and update all code files
    echo The first run
    # AllNodes=`cat ~/benchnodelist`
    # Will compile both antidoe and basho bench on all nodes in case the number changes in a later experiment
    AllNodes=`cat ~/fullnodelist`

  
    echo Perform configProxy.sh on "$BenchNode"

    echo First copying the node list to "$BenchNode"
    echo scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodesfull
    scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodesfull

    echo scp ~/basho_bench/script/configProxy.sh root@"$BenchNode":/root/basho_bench1/basho_bench/script/
    scp ~/basho_bench/script/configProxy.sh root@"$BenchNode":/root/basho_bench1/basho_bench/script/

    echo Now copying the cookie list to "$BenchNode"
    echo scp ~/allcookielist root@"$BenchNode":/root/basho_bench1/basho_bench/script/allcookiesfull
    scp ~/allcookielist root@"$BenchNode":/root/basho_bench1/basho_bench/script/allcookiesfull

    echo ssh root@$BenchNode /root/basho_bench1/basho_bench/script/configProxy.sh
    ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/configProxy.sh


    for I in $(seq 1 $BenchParallel); do
	echo Checking out 
	Command0="cd ./basho_bench"$I"/basho_bench/  && rm -f ./script/configProxy.sh && git stash && git fetch && git checkout $GridBranch && git pull && rm -rf ./deps/* && make all"
	~/basho_bench/script/parallel_command.sh "$AllNodes" "$Command0" >> logs/"$GridJob"/basho_bench-compile-job"$Time"
    done

    echo Performins configMachines.sh on "$BenchNode"
    echo First copying the node list to "$BenchNode"
    echo scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodesfull
    scp ~/fullnodelistip root@"$BenchNode":/root/basho_bench1/basho_bench/script/allnodesfull
    echo ssh root@$BenchNode /root/basho_bench1/basho_bench/script/configMachines.sh $Branch
    ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/configMachines.sh $Branch $GridJob $Time

    # Copy the allnodes file to the benchmark locations
    echo all nodes are `cat ~/nodelistip`
    echo Performing SCPs
    for Node in `cat ~/benchnodelist`; do
	for I in $(seq 1 $BenchParallel); do
	    echo scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    scp ~/nodelistip root@"$Node":/root/basho_bench"$I"/basho_bench/script/allnodes
	    echo scp ~/computecookielist root@"$Node":/root/basho_bench"$I"/basho_bench/script/allcookies
	    scp ~/computecookielist root@"$Node":/root/basho_bench"$I"/basho_bench/script/allcookies
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
	    echo scp ~/computecookielist root@"$Node":/root/basho_bench"$I"/basho_bench/script/allcookies
	    scp ~/computecookielist root@"$Node":/root/basho_bench"$I"/basho_bench/script/allcookies
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
echo Running the test at $BenchNode
echo ssh -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/runMultipleTests.sh $TotalDCs $Size $BenchParallel $BenchCount $GridJob $Time $BenchFile
ssh -t -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/runMultipleTests.sh $TotalDCs $Size $BenchParallel $BenchCount $GridJob $Time $BenchFile

# Get the results

Reads=( 99 90 75 50 )

echo Compiling the results
cd ~

mkdir antidote_bench-"$Time"
touch ~/antidote_bench-"$Time"/filenames

for ReadWrite in $(seq 0 3); do
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
    ./basho_bench/script/mergeResults.sh ~/antidote_bench-"$Time"/ "$BenchFile"-"${Reads[$ReadWrite]}" $Branch $BenchFile
    
done



echo Taring them to antidote_bench-"$Time".tar
tar cvzf antidote_bench-"$Time".tar antidote_bench-"$Time" >> logs/"$GridJob"/tar_merged_job"$Time"

echo antidote_bench-"$Time".tar antidote_bench-"$Time" >> tarnames
