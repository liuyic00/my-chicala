#!/bin/bash

sbt publishLocal "project testcase" clean 'set scalacOptions++=Seq("-P:chicala:useVecOnly", "-P:chicala:useBoolean", "-P:chicala:unbreakBlocks", "-P:chicala:useRecursiveFunc", "-P:chicala:useNestedCat", "-P:chicala:disableNeedCheckWarn")' compile
./scripts/format.sh
./scripts/compare.sh