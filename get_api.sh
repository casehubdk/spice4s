#!env bash
rm -rf ./proto/src/main/protobuf/*
curl -L https://github.com/authzed/api/archive/main.tar.gz -o ./proto/src/main/protobuf/api.tar.gz
tar -xvzf proto/src/main/protobuf/api.tar.gz -C proto/src/main/protobuf
mkdir -p ./proto/src/main/protobuf/authzed/api
mv ./proto/src/main/protobuf/api-main/authzed/api/* ./proto/src/main/protobuf/authzed/api
rm -rf ./proto/src/main/protobuf/authzed/**/*openapi.proto
rm -rf ./proto/src/main/protobuf/api.tar.gz
rm -rf ./proto/src/main/protobuf/api-main