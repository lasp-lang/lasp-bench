#!/bin/bash


ExternalIPs=$2

Nodes=`head -$1 ./script/allnodes`
First=`head -1 ./script/allnodes`

LocalExIP=`ssh -i key -o StrictHostKeyChecking=no root@$First "curl \"http://169.254.169.254/latest/meta-data/public-ipv4\""`
echo "Local ex ip is"$LocalExIP

Array=($ExternalIPs)
for IP in "${Array[@]}"
do
    if [ "$IP" != "$LocalExIP" ]
    then
        echo here
        NewExtIPs=$NewExtIps" "$IP
    fi
done

echo "Trying to connect" $Nodes "to" $NewExtIPs

erl -pa script -setcookie antidote -name setup@localhost -run connectDCs connect  $1 $Nodes  $NewExtIPs -run init stop
