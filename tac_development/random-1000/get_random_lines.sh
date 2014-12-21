#!/bin/bash

MAXCOUNT=10
count=1


while [ "$count" -le $MAXCOUNT ]
do
  number=$((RANDOM%1000))
  echo $number
  let "count += 1"
done
