#!/bin/sh

set -e
export TZ=UTC

DATE="$(date)"
MSG="Update $(date '+%F %T %Z' --date="$DATE")."

mkdir -p latest
guix time-machine -C channels.scm -- describe -f channels > channels.lock
guix time-machine -C channels.lock -- repl update.scm
cp channels.lock guix.txt guix-packages.txt nonguix-packages.txt latest

git add latest
git commit -m "$MSG"
git push
