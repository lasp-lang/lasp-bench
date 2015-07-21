#!/bin/bash

FAIL=0
Nodes=$1
File=$2
Folder=$3
echo "Fetching "$File" from" $Nodes 
for Node in $Nodes
do
   NewDir=$Folder"/"$Node
   mkdir $NewDir
   scp -i key -o StrictHostKeyChecking=no -r root@$Node:$File $NewDir
done
echo $command done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
echo "$command finished." 
else
echo "Fail! ($FAIL)"
fi
