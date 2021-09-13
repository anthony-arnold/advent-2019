#!/usr/bin/awk -f
{
   gsub("[<>=,x-z]","")
  print $0
}
