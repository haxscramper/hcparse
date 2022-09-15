#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

clang++ libclang-3.cpp -lLLVM -lclang-cpp -olibclang-3
./libclang-3
