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
    for (i = 4; i <= 10; i++) {
	a[i] = a[i] - last[i]
    }
    for (i = 4; i <= 10; i++) {
	a[i] = a[i] / max
	if(i != 4)
	    printf ", "
	printf "%s",a[i] / 1000
    }
    print ""
}
