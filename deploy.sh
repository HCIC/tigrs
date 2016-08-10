#!/bin/bash -e
echo "starting full deploy..."
./build-production.sh
./prepare-data.sh
./deploy-data.sh
./deploy-site.sh
