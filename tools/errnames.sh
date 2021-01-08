#!/bin/bash
set -o pipefail

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

[[ -z "$CC" ]] && CC=cc

function generate-source {
    echo '#include "errno_utils.h"'
    echo 'int main() {'
    echo 'print_header();'

    echo '#include <errno.h>' \
        | $CC -dM -E - \
        | awk /"$REGEX"/'{print $(NF),$(NF-1)}' \
        | sed -E -e 's/^[0-9]+ //' -e 's/[[:space:]]+/\n/g' \
        | sed -e s/'^'/"$PREFIX"/ -e s/'$'/"$SUFFIX"/

    echo 'print_trailer();'
    echo 'return 0;'
    echo '}'
}

generate-source

## https://stackoverflow.com/a/19885466
