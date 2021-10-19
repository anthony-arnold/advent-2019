#!/usr/bin/awk -f
/cut/ { print $0 }
/new stack/ { print "new 0" }
/increment/ { print "incr "$4 }
