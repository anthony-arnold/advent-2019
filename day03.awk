#!/usr/bin/awk -f
{
   split($1,a,",");
   for (i in a) print substr(a[i], 0,1)" "substr(a[i], 2);
   print "- 0"
}
