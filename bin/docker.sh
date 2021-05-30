#!/bin/bash


if [[ "$1" = "build" ]]; then
  set -x
  docker build -t slackat:latest .
elif [[ "$1" = "run" ]]; then
  set -x
  docker run -it -p 5432:5432 -p 3003:3003 --env-file .env.local --rm slackat:latest
else
  echo "$0 build  OR  $0 run"
fi
