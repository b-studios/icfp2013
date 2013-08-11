#!/bin/bash -x

baseUrl="http://plse.informatik.uni-marburg.de:8888/"
# In seconds
pingInterval=5

id=$1

# Ensure that Ctrl-C kills the shell, instead of letting it self-restart.
trap "exit 1" EXIT INT

getErrorCode() {
  url="$1"

  # -I : use HEAD
  # -s : silent
  # -o /dev/null : more silent (the output goes to /dev/null)
  # -w '%{http_code}' : printf just the HTTP response code
  curl -o /dev/null -s -I -w '%{http_code}' "$url"
}

mustKill() {
  mustKillUrl="${baseUrl}stay-alive/${id}"
  code=$(getErrorCode "$mustKillUrl")
  if [ "$code" = 200 ]; then
    # Do nothing
    echo 0
  elif [ "$code" = 410 ]; then
    # Kill
    echo 1
  elif [ "$code" = 000 ]; then
    echo ">>>> Server not running! Exiting." >&2
    # Kill && exit.
    echo 2
  else
    # Kill anyway
    echo 1
  fi
}

runSubProcess() {
  bash -c 'echo $$; exec '"$* >> output-$id.txt 2>&1" | {
  # bash -c 'echo $$; exec '"$* >&2" | {
    read pid;
    while :; do
      sleep ${pingInterval}
      res=$(mustKill)
      [ "${res}" -eq 1 -o "${res}" -eq 2 ] && { kill $pid; break; }
    done
    [ "${res}" -eq 2 ] && exit 2
    exec ./$(basename "$0") $id
  }
}

cd "$(dirname "$0")"
exe=./dist/build/lahnparty-run-worker/lahnparty-run-worker
# This runs the Haskell executable with the given ID.
runSubProcess ${exe} live ${id}
