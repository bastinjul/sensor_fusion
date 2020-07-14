#!/bin/bash

mkdir "experiments/$1" "experiments/$1/measures" "experiments/$1/calculations"
# shellcheck disable=SC2045
for i in $(ls measures/*.1)
do
  cp "$i" "experiments/$1/measures"
  true > "$i"
done
# shellcheck disable=SC2045
for i in $(ls calculations/*.1)
do
  cp "$i" "experiments/$1/calculations"
  true > "$i"
done

