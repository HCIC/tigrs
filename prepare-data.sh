#!/bin/bash
mkdir -p data
echo "downloading..."
wget -nv --timestamping -i data-urls -P data

sbt run
echo "compressing..."
PATH=.:$PATH zopfli --i1 data/fakall.boo
ls -lh data
