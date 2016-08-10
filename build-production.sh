#!/bin/bash -e

NAME="tigrs"

echo "building production assets..."
sbt fullOptJS

rm -rf out
mkdir -p out
cp js/target/scala-2.11/{classes/index.html,$NAME-{jsdeps.min.js,opt.js,launcher.js}} out

echo "compressing..."
zopfli out/*

cp htaccess-multiviews out/.htaccess
