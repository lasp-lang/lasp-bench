#!/bin/bash

cd /root/basho_bench1/basho_bench/
mkdir logs

AllNodes=`cat /root/basho_bench1/basho_bench/script/allnodes`
echo All nodes "$AllNodes"

Command00="sed -i '/export http_proxy/d' ~/.bashrc"
Command01="sed -i '/export https_proxy/d' ~/.bashrc"
Command02="sed -i '10i export https_proxy=https://proxy.sophia.grid5000.fr:3128/' ~/.bashrc"
Command03="sed -i '10i export http_proxy=http://proxy.sophia.grid5000.fr:3128/' ~/.bashrc"

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
