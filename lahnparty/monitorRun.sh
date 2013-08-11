#!/bin/bash -v -x

baseUrl="http://plse.informatik.uni-marburg.de:8888/"
id=$1

getErrorCode() {
  url="$1"

  # -I : use HEAD
  # -s : silent
  # -o /dev/null : more silent (the output goes to /dev/null)
  # -w '%{http_code}' : printf just the HTTP response code
  curl -o /dev/null -s -I -w '%{http_code}' "$url"
}

mustKill() {
  mustKillUrl="${baseUrl}/stay-alive/${id}"
  code=$(getErrorCode "$mustKillUrl")
  if [ "$code" = 200 ]; then
    # Do nothing
    echo 0
  elif [ "$code" = 410 ]; then
    # Kill
    echo 1
  else
    # Kill anyway
    echo 1
  fi
}

runSubProcess() {
  bash -c 'echo $$; exec '"$* >&2" | {
    read pid;
    while :; do
      sleep 5
      [ "$(mustKill)" -eq 1 ] && { kill $pid; return; }
    done
  }
}

exe=./lahnparty/dist/build/lahnparty-run-worker/lahnparty-run-worker
# This runs the Haskell executable with the given ID.
runSubProcess ${exe} train ${id}
