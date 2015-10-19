#!/bin/awk -f
BEGIN {
    print "# min, mean, median, 95th, 99th, 99_9th, max"
}
NR!=1 {
    printf "%s, %s, %s, %s, %s, %s, %s, %s\n", NR-1, $4/1000, $5/1000, $6/1000, $7/1000, $8/1000, $9/1000, $10/1000
}
