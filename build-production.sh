#!/bin/bash -e

NAME="frontend"

echo "building production assets..."
sbt fullOptJS

rm -rf out
mkdir -p out
cp index.html out
cp $NAME/target/scala-2.11/$NAME-{jsdeps.min.js,opt.js,launcher.js} out

echo "compressing..."
zopfli out/*

cp htaccess-multiviews out/.htaccess
