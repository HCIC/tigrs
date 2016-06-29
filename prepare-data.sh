#!/bin/bash
mkdir -p data
echo "downloading..."
wget -nv --timestamping -i data-urls -P data
echo "compressing..."
PATH=.:$PATH zopfli --i1 data/*.xml
ls -lh data
