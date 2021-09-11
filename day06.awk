#!/usr/bin/awk -f
{
   gsub("\\)", " ")
  print $0
}
