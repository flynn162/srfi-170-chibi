#!/bin/bash

maybeSpace='[[:space:]]*'
atLeastOneSpace='[[:space:]]+'
errorName='E[A-Z_0-9]+'
numbers='[0-9]+'

REGEX="^${maybeSpace}#${maybeSpace}"
REGEX+="define${atLeastOneSpace}"
REGEX+="${errorName}${atLeastOneSpace}"
REGEX+="(${errorName}|${numbers})"
REGEX+="${maybeSpace}"'$'

PREFIX='register_errno('
SUFFIX=');'

function generate-source {
    echo '#include "errno_utils.h"'
    echo 'int main() {'
    echo 'print_header();'

    echo '#include <errno.h>' \
        | env -- "$@" -dM -E - \
        | awk /"$REGEX"/'{print $(NF),$(NF-1)}' \
        | sed -E -e 's/^[0-9]+ //' -e $'s/[[:space:]]+/\\\n/g' \
        | sed -e s/'^'/"$PREFIX"/ -e s/'$'/"$SUFFIX"/

    echo 'print_trailer();'
    echo 'return 0;'
    echo '}'
}

set -e -o pipefail
if [[ $# = 0 ]]; then
    generate-source cc
elif [[ "$1" != "--" ]]; then
    >&2 echo "Syntax Error: The first argument must be --"
    exit 99
elif [[ $# = 1 ]]; then
    >&2 echo "STOP: No parameters were given after --"
    exit 99
else
    shift
    generate-source "$@"
fi

## https://stackoverflow.com/a/19885466
