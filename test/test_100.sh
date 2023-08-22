#!/bin/sh

cat /dev/random | \
    head -c 3145728 | \
    curl -v -X PUT http://localhost:8000 --data-binary @-
