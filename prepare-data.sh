#!/bin/bash -e
echo "preparing data..."
mkdir -p data
echo "downloading..."
wget -nv --timestamping -i data-urls -P data

echo "transforming..."
sbt run
echo "compressing..."
PATH=.:$PATH zopfli --i1 data/fakall.boo
ls -lh data

mkdir out/data
cp data/fakall.boo.gz out/data
