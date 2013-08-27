#!/bin/bash

rm -rf ~/.cache/common-lisp/
rm -rf ~/quicklisp-controller/dist
sbcl --no-userinit --no-sysinit  --load ~/quicklisp/setup.lisp \
    --load rerun.lisp
