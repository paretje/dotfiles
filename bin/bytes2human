#!/usr/bin/awk -f
{
	sum=$1
	hum[1073741824] = "GiB"
	hum[1048576]    = "MiB"
	hum[1024]       = "KiB"
	hum[1]          = "bytes"
	for (x=1073741824; x>=1; x/=1024)
	{
		if (sum>=x)
		{
			printf "%.2f %s\n", sum/x, hum[x]
			break
		}
	}
}
