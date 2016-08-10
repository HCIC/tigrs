#!/bin/bash -e
echo "preparing data..."

echo "downloading..."
wget -nv --timestamping -i data-urls -P data

echo "transforming and compressing..."
cd data
    make
cd ..

ls -lh data

mkdir -p out/data
cp data/fakall.boo.gz out/data
