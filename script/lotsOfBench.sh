#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage configlefile whit lines gridjobid branch dodeploy secondrun computeCount benchCount benchParallel dcspercluster benchBranch benchFile"
    exit
fi

ConfigFile=$1

# for dcs in 6 4 2
# do
#     nodes=12
#     benchnodes=3
#     sed -i '5s/.*/'$nodes'/' $1
#     sed -i '6s/.*/'$benchnodes'/' $1
#     sed -i '8s/.*/'$dcs'/' $1
#     echo new config
#     cat $1
#     echo
#     ~/basho_bench/script/grid5000start.sh $1
# done

for dcs in 2
do
    for nodes in 40
    do
	benchnodes=$(($nodes / 4))
	sed -i '5s/.*/'$nodes'/' $1
	sed -i '6s/.*/'$benchnodes'/' $1
	sed -i '8s/.*/'$dcs'/' $1
	echo new config
	cat $1
	echo
	~/basho_bench/script/grid5000start.sh $1
	sed -i '3s/.*/0/' $1
	sed -i '4s/.*/1/' $1
    done
done

for dcs in 8 10
do
    for nodes in 8
    do
	benchnodes=$(($nodes / 4))
	sed -i '5s/.*/'$nodes'/' $1
	sed -i '6s/.*/'$benchnodes'/' $1
	sed -i '8s/.*/'$dcs'/' $1
	echo new config
	cat $1
	echo
	~/basho_bench/script/grid5000start.sh $1
	sed -i '3s/.*/0/' $1
	sed -i '4s/.*/1/' $1
    done
done

for dcs in 1 2 4
do
    for nodes in 20
    do
	benchnodes=$(($nodes / 4))
	sed -i '5s/.*/'$nodes'/' $1
	sed -i '6s/.*/'$benchnodes'/' $1
	sed -i '8s/.*/'$dcs'/' $1
	echo new config
	cat $1
	echo
	~/basho_bench/script/grid5000start.sh $1
	sed -i '3s/.*/0/' $1
	sed -i '4s/.*/1/' $1
    done
done
