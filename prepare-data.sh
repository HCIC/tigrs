#!/bin/bash -e
echo "preparing data..."

echo "downloading..."
wget -nv --timestamping -i data-urls -P data

(
    echo "transforming..."
    export SBT_OPTS="-Xmx2048M"
    sbt "modsParser/run"

    cd data
    echo "compressing..."
    # zopfli fakall.ikz.*.boo
    find -name 'fakall.ikz.*.boo' -print0 | xargs -0 --max-args=1 --max-procs=16 zopfli
    zopfli fakall.ikzlist.boo
);

mkdir -p out/data
cp data/*.boo.gz out/data
