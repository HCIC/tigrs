#!/bin/bash -e

NAME="frontend"

echo "building production assets..."
sbt "frontend/fullOptJS"

rm -rf out
mkdir -p out
cp index.html out
cp $NAME/target/scala-2.12/$NAME-{jsdeps.min.js,opt.js} out

echo "compressing..."
zopfli out/*

cp htaccess-multiviews out/.htaccess
