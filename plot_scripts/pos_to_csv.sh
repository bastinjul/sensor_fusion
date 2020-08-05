#!/bin/bash

for i in $(ls ../experiments/)
do
  if [ -d "../experiments/$i/measures" ]
  then
    for j in $(ls ../experiments/$i/measures/pos*)
    do
      awk -F " " '{print $6 "," $8}' "$j" | awk -F "," '{print $1 "," $3}' | tr -d } > "$j.csv"
    done
  fi
done