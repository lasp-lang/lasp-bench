#!/bin/awk -f
BEGIN {
    FS=","
    max = 0
    filecount = 0
}
NR > 2 {
    for (i = 1; i <= NF; i++) {
	a[i] = $i + a[i]
	last[i] = $i
	max = NR - 3
    }
}
END {
    for (i = 3; i <= 5; i++) {
	a[i] = a[i] - last[i]
    }
    for (i = 3; i <= 5; i++) {
	a[i] = a[i] / max
	if(i != 3)
	    printf ", "
	printf "%s",a[i] / 10
    }
    print ""
}
