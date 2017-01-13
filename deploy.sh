#!/bin/bash -e
echo "starting full deploy..."
./deploy-data.sh
./deploy-site.sh
