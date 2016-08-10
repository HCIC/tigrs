#!/bin/bash -e
{
  echo put out/tigrs-jsdeps.min.js
  echo put out/tigrs-jsdeps.min.js.gz
  echo put out/tigrs-launcher.js
  echo put out/tigrs-launcher.js.gz
  echo put out/tigrs-opt.js
  echo put out/tigrs-opt.js.gz
  echo put out/index.html
  echo put out/index.html.gz
  echo put out/.htaccess
} | sftp -b - $DEPLOY_USER@$DEPLOY_HOST
