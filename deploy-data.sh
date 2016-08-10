#!/bin/bash -e
{
  echo progress
  echo -mkdir data
  echo -put data/fakall.boo.gz data
} | sftp -b - $DEPLOY_USER@$DEPLOY_HOST
