#!/bin/bash -e
echo "starting full deploy..."
export SBT_OPTS=-Xmx4048M
./deploy-data.sh
./deploy-site.sh
