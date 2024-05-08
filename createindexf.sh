#!/bin/bash

cd translated

echo "Creating an indexfile for every .mcrl file in translated..."

find -name "*.mcrl2" > "index.txt"

echo "Done"

