#!/bin/bash

dir1="test_run_dir/chiselToScala/last"
dir2="test_run_dir/chiselToScala/out"

find "$dir1" -type f | while read file1; do
    relative_path="${file1#$dir1/}"
    file2="$dir2/$relative_path"

    if [ -f "$file2" ]; then
        if cmp -s "$file1" "$file2"; then
            echo "$relative_path is identical."
        else
            echo "x $relative_path is different."
            code --diff "$file1" "$file2" &
        fi
    fi
done