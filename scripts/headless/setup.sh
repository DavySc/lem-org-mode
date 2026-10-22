#!/bin/sh
# Build a Lem without a GUI so that the Rove tests and the smoke test can run in a terminal or a container.
#
#   scripts/headless/setup.sh [DIR]      (default DIR: $HEADLESS_DIR or /tmp/lem-org-headless)
#
# It needs git, make, python3 and sbcl, and network access to github.com. Quicklisp is not used (it is blocked
# in some sandboxes): the Lisp libraries Lem needs come from Debian packages when available and from git otherwise.
# Lem's own lem.asd is patched in the checkout: the GUI application system and the Quicklisp-only extension manager
# are dropped, because they pull in libraries that headless tests do not need.
set -e
DIR=${1:-${HEADLESS_DIR:-/tmp/lem-org-headless}}
mkdir -p "$DIR/cl"

if command -v apt-get >/dev/null 2>&1 && [ "$(id -u)" = 0 ]; then
  apt-get update -qq
  apt-get install -y -qq sbcl git make python3 cl-alexandria cl-ppcre cl-ppcre-unicode cl-unicode cl-iterate \
    cl-closer-mop cl-trivial-gray-streams cl-babel cl-bordeaux-threads cl-yason cl-split-sequence cl-mustache \
    cl-usocket cl-flexi-streams cl-cffi cl-trivial-features cl-trivial-garbage cl-global-vars cl-nibbles cl-quri \
    cl-trivial-utf-8 cl-chipz cl-base64 cl-utilities
fi

clone() { # clone OWNER/REPO [DIRNAME]
  name=${2:-$(basename "$1")}
  [ -d "$DIR/cl/$name" ] || git clone -q --depth 1 "https://github.com/$1" "$DIR/cl/$name"
}
clone tokenrove/anaphora;        clone sionescu/bordeaux-threads;   clone edicl/chunga
clone cl-plus-ssl/cl-plus-ssl cl+ssl;                               clone rudolfochrist/cl-change-case
clone fukamachi/cl-cookie;       clone fukamachi/dexador;           clone Shinmera/dissect
clone fukamachi/fast-http;       clone rpav/fast-io;                clone t-sin/inquisitor
clone Bike/introspect-environment; clone lem-project/lem-mailbox;   clone guicho271828/lisp-namespace
clone dlowe-net/local-time;      clone sharplispers/log4cl;         clone lem-project/micros
clone fukamachi/proc-parse;      clone oconnore/queues;             clone fukamachi/rove
clone fukamachi/smart-buffer;    clone sionescu/static-vectors;     clone vindarel/cl-str str
clone guicho271828/trivia;       clone Shinmera/trivial-mimes;      clone m2ym/trivial-types
clone guicho271828/type-i;       clone fukamachi/xsubseq
[ -d "$DIR/lem" ] || git clone -q --depth 1 https://github.com/lem-project/lem "$DIR/lem"

# Two libraries that are not on GitHub under a name that resolves: SBCL provides the same functionality.
mkdir -p "$DIR/cl/trivial-cltl2" "$DIR/cl/bt-semaphore"
cat > "$DIR/cl/trivial-cltl2/trivial-cltl2.asd" <<'LISP'
(defsystem "trivial-cltl2" :components ((:file "package")))
LISP
cat > "$DIR/cl/trivial-cltl2/package.lisp" <<'LISP'
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-cltl2))
(defpackage :trivial-cltl2 (:use :cl))
(in-package :trivial-cltl2)
(do-external-symbols (s :sb-cltl2) (shadowing-import s) (export s))
LISP
cat > "$DIR/cl/bt-semaphore/bt-semaphore.asd" <<'LISP'
(defsystem "bt-semaphore" :components ((:file "package")))
LISP
cat > "$DIR/cl/bt-semaphore/package.lisp" <<'LISP'
(defpackage :bt-semaphore (:use :cl)
  (:import-from :sb-thread :semaphore :make-semaphore :signal-semaphore :wait-on-semaphore :try-semaphore :semaphore-count)
  (:export :semaphore :make-semaphore :signal-semaphore :wait-on-semaphore :try-semaphore :semaphore-count))
LISP

# Patch lem.asd (idempotent).
python3 - "$DIR/lem/lem.asd" <<'PY'
import sys
path = sys.argv[1]
s = open(path).read()
if '(defsystem "lem" :depends-on ())' not in s:
    s = s.replace('               #-nix-build\n               "lem-extension-manager"\n', '')
    s = s[:s.index('(defsystem "lem"\n')] + '\n(defsystem "lem" :depends-on ())\n'
    open(path, 'w').write(s)
PY

# Lem uses package-local nicknames, which need ASDF 3.3.6 or newer; Debian's SBCL bundles an older one.
if ! sbcl --noinform --non-interactive --eval '(require :asdf)' \
     --eval '(sb-ext:exit :code (if (uiop:version<= "3.3.6" (asdf:asdf-version)) 0 1))' >/dev/null 2>&1; then
  [ -d "$DIR/asdf-src" ] || git clone -q --depth 1 https://github.com/fare/asdf "$DIR/asdf-src"
  (cd "$DIR/asdf-src" && make build/asdf.lisp >/dev/null)
  ASDF="(load \"$DIR/asdf-src/build/asdf.lisp\")"
else
  ASDF='(require :asdf)'
fi

cat > "$DIR/load-lem.lisp" <<LISP
$ASDF
(asdf:initialize-source-registry
 '(:source-registry (:tree "$DIR/cl/") (:tree "$DIR/lem/") (:directory "/usr/share/common-lisp/systems/") :inherit-configuration))
(handler-case (asdf:load-system "lem/core")
  (error (e) (format t "~%LOAD-ERROR: ~A~%" e) (sb-ext:exit :code 3)))
(asdf:load-system "rove")
LISP
sbcl --noinform --non-interactive --load "$DIR/load-lem.lisp" \
  --eval "(sb-ext:save-lisp-and-die \"$DIR/lem.core\")" > "$DIR/build.log" 2>&1 || { tail -20 "$DIR/build.log"; exit 1; }
echo "headless Lem ready: $DIR/lem.core"
