#!/usr/bin/awk  -f
BEGIN { FS="," }
{
   print NF
   gsub("=>", "")
   print $0
}
