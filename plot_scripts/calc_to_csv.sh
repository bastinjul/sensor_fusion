#!/bin/bash

for i in $(ls ../experiments/)
do
  if [ -d "../experiments/$i/calculations" ]
  then
    for j in $(ls ../experiments/$i/calculations)
    do
      tr -d "{[]}" < "../experiments/$i/calculations/$j" > "../experiments/$i/calculations/$j.csv"
      sed -i '1s/^/iteration,x-axis,y-axix,x-axis,y-axix\n/' "../experiments/$i/calculations/$j.csv"
    done
  fi
done