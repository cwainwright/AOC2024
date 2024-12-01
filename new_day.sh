#!/bin/bash

# Parameters
CABAL_FILE="AOC2024.cabal"
DIST_DIR="dist-newstyle"
DAY=$(date '+%d' | sed 's/^0//')
NEW_FILE="src/Day${DAY}.hs"

# Create the new Haskell file
if [[ -e "$NEW_FILE" ]]; then
  echo "File $NEW_FILE exists"
else 
  echo "module Day${DAY} where" > $NEW_FILE
  echo "Created $NEW_FILE."
fi

# Update the .cabal file
if ! grep -q "Day${DAY}" "$CABAL_FILE"; then
    sed -i "/exposed-modules:/ a\    Day${DAY}" "$CABAL_FILE"
    echo "Added Day${DAY} to $CABAL_FILE."
else
    echo "Module Day${DAY} is already in $CABAL_FILE."
fi

# Remove the dist-newstyle folder
rm -rf "$DIST_DIR"
echo "Deleted $DIST_DIR."

if cabal build; then
  echo "Cabal Build Complete"
else
  echo "Cabal Build Failed"
fi

echo "Setup for Day ${DAY} complete."