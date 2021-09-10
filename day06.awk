#!/usr/bin/awk -f
{
  gsub("("," ",$1)
  print $1
}
