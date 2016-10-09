#!/usr/bin/env sh

mkdir -p backups/
for i in $@ ; do
  TARGET="backups/$i-`date -I`"
  cp "$i" "$TARGET";
  gzip "$TARGET";
done

