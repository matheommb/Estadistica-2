#!/bin/bash

# loop through all subdirectories that start with "Taller"
for dir in Taller*; do
  # check if the directory name contains "_"
  if [[ "$dir" == *"_"* ]]; then
    # extract the Taller# part of the name
    new_dir=$(echo "$dir" | cut -d'_' -f1)
    # rename the directory
    mv "$dir" "$new_dir"
    echo "Nombre cambiado"
  fi
done

