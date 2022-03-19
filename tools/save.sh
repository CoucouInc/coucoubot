#!/usr/bin/env sh

mkdir -p backups/
for i in $@ ; do
  TARGET="backups/`basename $i .db`-`date -I`.db"
  sqlite3 "$i" "VACUUM INTO '${TARGET}';"
  gzip "$TARGET";
done

