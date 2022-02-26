#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

SRC_DIR=$(realpath "$(dirname "${BASH_SOURCE[0]}")")
SKULL=$SRC_DIR/../nimskull

nim c \
    --lib=$SKULL/lib \
    --define=useNodeIds \
    --debugger=native \
    --excludePath=$HOME/.choosenim/toolchains/nim-1.6.4 \
    -d=nimDebugUtils \
    --path=$SKULL \
    $@
