#!/bin/awk -f
BEGIN {
    print "# total successful failed"
}
NR!=1 {
    printf "%s, %s, %s, %s\n", NR-1, $3/$2, $4/$2, $5/$2
}
