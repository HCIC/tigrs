#!/bin/bash -e
{
  echo put out/frontend-jsdeps.min.js
  echo put out/frontend-jsdeps.min.js.gz
  echo put out/frontend-launcher.js
  echo put out/frontend-launcher.js.gz
  echo put out/frontend-opt.js
  echo put out/frontend-opt.js.gz
  echo put out/index.html
  echo put out/index.html.gz
  echo put out/.htaccess
} | sftp -b - $DEPLOY_USER@$DEPLOY_HOST
