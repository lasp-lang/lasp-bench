#!/bin/bash

$BenchNode=`cat tmpTime`

echo Stopping time stabalize at $BenchNode
echo ssh -o StrictHostKeyChecking=no root@$BenchNode /root/basho_bench1/basho_bench/script/stopStabalize.sh
