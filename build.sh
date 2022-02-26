#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

./compile.sh \
    -o=hc_tsreader.bin \
    ./src/hcparse/read_tree_sitter/hc_tsconvert.nim

./hc_tsreader.bin
