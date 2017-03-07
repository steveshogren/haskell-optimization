#!/bin/sh

(
  declare -i p
  trap 'kill "$p"' EXIT
  while true; do
    stack exec paragon-optimization & p=$!
    notify-send "Server started" --urgency=critical
    inotifywait -q -e create .stack-work/install/*/*/*/bin/
    sleep 1
    kill "$p"
  done
)
