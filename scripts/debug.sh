#!/bin/bash

make test-debug
./scripts/format.sh
./scripts/compare.sh
