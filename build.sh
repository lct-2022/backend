#!/bin/bash

VERSION=latest

export DOCKER_BUILDKIT=1

set -ex

docker build --add-host 'beta.quicklisp.org:13.33.243.6' --progress plain --tag svetlyak40wt/app-back:$VERSION -f Dockerfile.base .

docker push svetlyak40wt/app-back:$VERSION

docker build --tag test-back-image .

