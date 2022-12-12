#!/bin/sh

cookie="$(cat cookie)"
year=2022
day=$1

if [ -z "$day" ]; then echo "missing day"; exit 1; fi

curl -s --cookie "$cookie" https://adventofcode.com/${year}/day/${day} || exit 1
