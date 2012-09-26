#!/bin/bash

for i in {1..1000}
do
  $1 $2 > /dev/null
done
