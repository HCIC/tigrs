#!/bin/bash -e
echo "preparing data..."

echo "downloading..."
wget -nv --timestamping -i data-urls -P data

cd data
    echo "transforming..."
    make
    echo "compressing..."
    zopfli fakall.ikz.*.boo
    zopfli fakall.ikzlist.boo
cd ..

ls -lh data

mkdir -p out/data
cp data/*.boo.gz out/data
