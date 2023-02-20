#!/bin/bash

# Loop over all directories in the current directory
for dir in */; do
    # Check if the directory is a git repository
    if [ -d "$dir/.git" ]; then
        # Change into the directory and perform a git pull
        cd "$dir"
        git pull
        cd ..
    fi
done
