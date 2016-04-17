#!/bin/bash

cd /root/basho_bench1/basho_bench/
mkdir logs

AllNodes=`cat /root/basho_bench1/basho_bench/script/allnodesfull`
echo All nodes "$AllNodes"

Command00="sed -i '/export http_proxy/d' ~/.bashrc"
Command01="sed -i '/export https_proxy/d' ~/.bashrc"
Command02="sed -i '10i export https_proxy=https://proxy.sophia.grid5000.fr:3128/' ~/.bashrc"
Command03="sed -i '10i export http_proxy=http://proxy.sophia.grid5000.fr:3128/' ~/.bashrc"

# echo Copying experiment keys
# for node in $AllNodes
#do
#   scp -t -o ConnectTimeout=3 -o StrictHostKeyChecking=no -t key root@"$node":~/basho_bench1/basho_bench/ -i key ${command/localhost/$node}
#   scp -t -o ConnectTimeout=3 -o StrictHostKeyChecking=no -t key root@"$node":~/basho_bench2/basho_bench/ -i key ${command/localhost/$node}
#   scp -t -o ConnectTimeout=3 -o StrictHostKeyChecking=no -t key root@"$node":~/basho_bench3/basho_bench/ -i key ${command/localhost/$node}
#   scp -t -o ConnectTimeout=3 -o StrictHostKeyChecking=no -t key root@"$node":~/basho_bench4/basho_bench/ -i key ${command/localhost/$node}
#done


echo Running proxy config
echo

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command00"	
./script/parallel_command.sh "$AllNodes" "$Command00" >> logs/config_proxy

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command01"	
./script/parallel_command.sh "$AllNodes" "$Command01" >> logs/config_proxy

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command02"	
./script/parallel_command.sh "$AllNodes" "$Command02" >> logs/config_proxy

echo Performing: ./script/parallel_command.sh "$AllNodes" "$Command03"	
./script/parallel_command.sh "$AllNodes" "$Command03" >> logs/config_proxy
