#!/bin/bash -e

NAME="frontend"

echo "building production assets..."
sbt clean "frontend/fullOptJS::webpack"

rm -rf out
mkdir -p out
cp index.html out
cp $NAME/target/scala-2.11/scalajs-bundler/main/frontend-opt-bundle.js out

echo "compressing..."
zopfli out/*

cp htaccess-multiviews out/.htaccess
