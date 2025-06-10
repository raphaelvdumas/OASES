#!/bin/sh
# Create machine dependent object and binary directories for CSNAP
#
  if [ ! -d ./${1} ]; then
    mkdir ./${1}
  fi

