#!/bin/bash

git pull origin master
sbt assembly
docker build -t anonreal/ac-off-chain .

cd ~/services
docker stop ac && docker rm ac
docker-compose up -d
