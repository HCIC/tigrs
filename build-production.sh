#!/bin/bash -e

echo "building production assets..."
sbt clean "frontendAssets/assets"

rm -rf out
mkdir -p out
cp index.html out
cp frontendAssets/target/web/public/main/frontend-opt-bundle.js out

echo "compressing..."
zopfli out/*

cp htaccess-multiviews out/.htaccess
