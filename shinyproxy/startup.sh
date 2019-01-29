#!/bin/bash

# Set API_IP if set as ENV in compose file
# If not set Find HOST ip and used localhost as IP
if [ ! -z "$API_IP" ]; then
    sed -i "s|%API_IP%|$API_IP|g" settings.env
else
    sed -i "s|%API_IP%|$(ip route show | awk '/default/ {print $3}')|" settings.env
#sed -i "s|%DOCKER_HOST_IP%|$(ip route show | awk '/default/ {print $3}')|" settings.env
fi 

sed -i "s|%UI_DOCKER_IMAGE%|$UI_DOCKER_IMAGE|g" application.yml
sed -i "s|%API_PORT%|$API_PORT|g" settings.env
sed -i "s|%API_VERSION%|$API_VERSION|g" settings.env
sed -i "s|%API_SHARE_FILEPATH%|$API_SHARE_FILEPATH|g" settings.env
sed -i "s|%OASIS_ENVIRONMENT%|$OASIS_ENVIRONMENT|g" settings.env

java -jar shinyproxy-1.1.0.jar
