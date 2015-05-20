#!/bin/bash

ExternalIPs=$2

Nodes=`head -$1 ./script/allnodes`
First=`head -1 ./script/allnodes`

LocalExIP=`./script/command_to_all.sh $First curl "http://169.254.169.254/latest/meta-data/public-ipv4"`

for IP in "${ExternalIPs[@]}"
do
    if [ "$IP" = "$LocalExIP" ]
    then
        NewExtIPs=$NewExtIps" "$IP
    fi 
done

echo "$Nodes"
echo "$NewExtIP"

erl -pa script -run connDCFun connect  $3 $Nodes  $NewExtIP
