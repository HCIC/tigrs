#!/bin/bash -e

./prepare-data.sh
{
  echo 'progress'
  echo '-rm data/*'
  echo '-mkdir data'
  echo 'put -R data/*.gz data'
  echo 'put -R data/*.boo data'
} | sftp -b - $DEPLOY_USER@$DEPLOY_HOST
