#!/bin/bash -e

./prepare-data.sh
{
  echo 'progress'
  echo '-rm data/*'
  echo '-mkdir data'
  echo 'put -R data/*.gz data'
} | sftp -b - $DEPLOY_USER@$DEPLOY_HOST
