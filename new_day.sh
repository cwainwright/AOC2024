#!/bin/bash

# Parameters
CABAL_FILE="AOC2024.cabal"
DIST_DIR="dist-newstyle"

if [[ -n "$1" ]]; then
  DAY="$1"
else
  DAY=$(date '+%d' | sed 's/^0//')
fi

# Cast to Integer
DAY=$((DAY))

HS_FILE="src/Day${DAY}.hs"
# Create the new Haskell file
if [[ -e "$HS_FILE" ]]; then
  echo "File $HS_FILE exists"
else
  { 
    echo "module Day${DAY} where"
    echo ""
    echo "main :: IO ()"
    echo "main = do"
    echo "  file_contents <- readFile \"data/D${DAY}.txt\""
    echo "  let file_lines = lines file_contents"
    echo "  mapM_ putStrLn file_lines"
  } >> $HS_FILE
fi

DATA_FILE="data/D${DAY}.txt"
# Create new Data File
if [[ -e "$DATA_FILE" ]]; then
  echo "File $DATA_FILE exists"
else
  touch $DATA_FILE
fi

# Update the .cabal file
if ! grep -q "Day${DAY}" "$CABAL_FILE"; then
    sed -i '' "/ *exposed-modules:.*/ a\\
      Day${DAY}\\
" "$CABAL_FILE"
    echo "Added Day${DAY} to $CABAL_FILE."
else
    echo "Module Day${DAY} is already in $CABAL_FILE."
fi

# Erase Existing Cache
cabal clean

# Rebuild Cache
if cabal build; then
  echo "Cabal Build Complete"
else
  echo "Cabal Build Failed"
fi

echo "Setup for Day ${DAY} complete."