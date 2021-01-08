#!/bin/sh

if [ $# -eq 0 ]; then
    >&2 echo "Transactional shell output writer"
    >&2 echo "Usage:"
    >&2 echo "  ./txshout.sh [outfile] -- [param] ..."
    >&2 echo "Example:"
    >&2 echo "  ./txshout.sh out.txt -- cat /etc/group"
    >&2 echo "  ./txshout.sh out.txt -- false"
    exit 10
fi

OUTPUT="$1"
TEMP1="$1"._1.tmp
TEMP2="$1"._2.tmp
shift

if [ ! "$1" = "--" ]; then
    >&2 echo "The second parameter must be --"
    exit 10
fi
shift

if [ $# -eq 0 ]; then
    >&2 echo "STOP: no parameters given"
    exit 10
fi

env -- "$@" < /dev/stdin > "$TEMP1"
ERROR=$?

if [ $ERROR -gt 0 ]; then
    >&2 echo "NOT WRITING: The process exited with status code" $ERROR
    rm -f "$TEMP1"
    exit $ERROR
fi

set -e
if [ -f "$TEMP2" ]; then rm "$TEMP2"; fi
if [ -f "$OUTPUT" ]; then mv "$OUTPUT" "$TEMP2"; fi
mv "$TEMP1" "$OUTPUT"
if [ -f "$TEMP2" ]; then rm "$TEMP2"; fi
