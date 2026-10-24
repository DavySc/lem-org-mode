#!/bin/sh
# Run the tests of lem-org-mode without a GUI.
#
#   scripts/headless/run.sh [pure] [rove] [smoke]      (default: all three)
#
# `pure' needs only SBCL. `rove' and `smoke' need the Lem built by setup.sh (DIR: $HEADLESS_DIR or
# /tmp/lem-org-headless). Every output line starting with FAIL, or an unexpected error, is a failure.
REPO=$(cd "$(dirname "$0")/../.." && pwd)
DIR=${HEADLESS_DIR:-/tmp/lem-org-headless}
[ $# -eq 0 ] && set -- pure rove smoke
status=0
for what in "$@"; do
  case $what in
    pure)
      echo "== pure functions"
      (cd "$REPO" && sbcl --script scripts/pure-test.lisp | grep -v '^PASS') || status=1 ;;
    rove)
      echo "== Rove tests"
      (cd "$REPO" && sbcl --core "$DIR/lem.core" --noinform --non-interactive \
        --eval "(progn (asdf:clear-configuration) (asdf:initialize-source-registry '(:source-registry (:tree \"$DIR/cl/\") (:tree \"$DIR/lem/\") (:directory \"$REPO/\") (:directory \"/usr/share/common-lisp/systems/\") :inherit-configuration)))" \
        --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "lem-org-mode/tests" :force t))' \
        --eval '(sb-ext:exit :code (if (rove:run "lem-org-mode/tests" :style :dot) 0 1))' > "${TMPDIR:-/tmp}/lem-org-rove.log" 2>&1
        status=$?
        sed 's/\x1b\[[0-9;]*m//g' "${TMPDIR:-/tmp}/lem-org-rove.log" | grep -a -v '^;\|^WARNING\|redefining'
        # a file that does not compile or read: show the compiler's message, which the filter above removes
        grep -a -A8 'caught ERROR' "${TMPDIR:-/tmp}/lem-org-rove.log" | head -40
        exit $status) || status=1 ;;
    smoke)
      echo "== smoke test"
      rm -f "$REPO/scripts/smoke.log"
      (cd "$REPO" && sbcl --core "$DIR/lem.core" --noinform --non-interactive \
        --eval "(progn (asdf:clear-configuration) (asdf:initialize-source-registry '(:source-registry (:tree \"$DIR/cl/\") (:tree \"$DIR/lem/\") (:directory \"$REPO/\") (:directory \"/usr/share/common-lisp/systems/\") :inherit-configuration)))" \
        --load scripts/smoke.lisp >/dev/null 2>&1)
      grep -v '^PASS' "$REPO/scripts/smoke.log"
      tail -1 "$REPO/scripts/smoke.log" | grep -q '^DONE' || status=1
      grep -q '^FAIL' "$REPO/scripts/smoke.log" && status=1 ;;
    *) echo "unknown: $what"; status=1 ;;
  esac
done
exit $status
