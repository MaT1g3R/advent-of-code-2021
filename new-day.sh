#!/bin/sh
cp -r dayx  "day$1"
cd "day$1" || exit
sd -s dayx "day$1" package.yaml 
stack init && stack build && gen-hie > hie.yaml
