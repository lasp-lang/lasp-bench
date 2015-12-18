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

for nodes in 6 3
do
    dcs=3
    benchnodes=$(($nodes / 3))
    sed -i '5s/.*/'$nodes'/' $1
    sed -i '6s/.*/'$benchnodes'/' $1
    sed -i '8s/.*/'$dcs'/' $1
    echo new config
    cat $1
    echo
    ~/basho_bench/script/grid5000start.sh $1
    sed -i '3s/.*/0/' $1
done
