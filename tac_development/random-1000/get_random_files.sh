#!/bin/bash

# must be used from machine kupo.

outdir=$(pwd)"/$1"
cd "/projects/WebWare6/DEFT_corpora/TAC_2010/TAC_2010_KBP_Source_Data/data/2009/nw"

filename="random-1000-KBP.txt"
while read line
do
    name=$line
		cp $name $outdir
done < "$filename"
