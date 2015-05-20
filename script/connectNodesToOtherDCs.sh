#!/bin/bash


ExternalIPs=$2

Nodes=`head -$1 ./script/allnodes`
First=`head -1 ./script/allnodes`

LocalExIP=`ssh -i key ubuntu@$First "curl \"http://169.254.169.254/latest/meta-data/public-ipv4\""`

Array=($ExternalIPs)
for IP in "${Array[@]}"
do
    if [ "$IP" = "$LocalExIP" ]
    then
        echo here
        NewExtIPs=$NewExtIps" "$IP
    fi
done


erl -noshell -pa script -setcookie antidote -name setup@localhost -run connectDCs connect  $1 $Nodes  $NewExtIPs
