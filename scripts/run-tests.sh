#!/usr/bin/env sh

set -e

LISP=${LISP:-sbcl}

_no_debug_flag=""
case "${LISP}" in
    sbcl)
        _no_debug_flag="--non-interactive"
        ;;
    ecl)
        _no_debug_flag="--nodebug"
        ;;
esac

${LISP} ${_no_debug_flag} \
        --eval '(ql:quickload :jingle.test)' \
        --eval '(setf rove:*enable-colors* nil)' \
        --eval '(asdf:test-system :jingle.test)' \
        --eval '(uiop:quit (length (rove/core/stats:all-failed-assertions rove/core/stats:*stats*)))'
