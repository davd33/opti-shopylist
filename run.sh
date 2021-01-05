#!/bin/bash

# Uncomment to enable database features
#export OPTI_SHOPYLIST_MANAGER="DATABASE"
# For db features, configure login and db name
#export OPTI_SHOPYLIST_DB_NAME="my-db-name"
#export OPTI_SHOPYLIST_USERNAME="db-user"
#export OPTI_SHOPYLIST_PASSWORD="password-for-db-user"
nohup ./opti-shopylist.ros &
