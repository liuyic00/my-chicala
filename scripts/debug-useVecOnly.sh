#!/bin/bash -e

make test-debug-useVecOnly
./scripts/format.sh
./scripts/compare.sh
