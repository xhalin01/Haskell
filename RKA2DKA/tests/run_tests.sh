#!/bin/bash

script_location=$(dirname $0)
path_to_executable="$script_location/../rka-2-dka"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

index=0
nextFile="$script_location/test$index.in"

while [ -e "$nextFile" ]; do

  solution="$script_location/test$index.out"
  
  output=$("$path_to_executable" -t "$nextFile")
  
  if [ $? -ne 0 ] || difference=$(diff --tabsize=15 --side-by-side <(echo "$output") "$solution"); then 
    echo -e "Test $index -${GREEN} OK ${NC}" >&2
  else 
    echo -e "Test $index -${RED} FAIL ${NC} Program's vs expected output:\n\n$difference\n" >&2
  fi

  index=$((index+1))
  nextFile="$script_location/test$index.in"

done



