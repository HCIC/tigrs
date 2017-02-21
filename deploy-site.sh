#!/bin/bash -e
./build-production.sh
{
  echo put out/frontend-opt-bundle.js
  echo put out/frontend-opt-bundle.js.gz
  echo put out/index.html
  echo put out/index.html.gz
  echo put out/.htaccess
} | sftp -b - $DEPLOY_USER@$DEPLOY_HOST
