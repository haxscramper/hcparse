#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

idxfile="../src/hcparse/libclang_raw/index.nim"

sed -i \
    "s/PriorityForAll = 0/PriorityForAll = 4/" \
    $idxfile



