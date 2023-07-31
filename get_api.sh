#!env bash
rm -rf ./proto/src/main/protobuf/*
mkdir -p ./proto/src/main/protobuf/authzed/api
curl -L https://github.com/authzed/api/archive/main.tar.gz -o ./proto/src/main/protobuf/api.tar.gz
tar -xvzf proto/src/main/protobuf/api.tar.gz -C proto/src/main/protobuf
mv ./proto/src/main/protobuf/api-main/authzed/api/* ./proto/src/main/protobuf/authzed/api
rm -rf ./proto/src/main/protobuf/authzed/api/v1/openapi.proto
rm -rf ./proto/src/main/protobuf/api.tar.gz
rm -rf ./proto/src/main/protobuf/api-main
