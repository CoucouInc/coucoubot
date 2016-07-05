#!/usr/bin/env sh

mkdir -p backups/
for i in $@ ; do
  cp "$i" "backups/$i-`date -I`";
done

