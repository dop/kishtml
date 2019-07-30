#!/bin/sh

fswatch -o . -l 1 | while read num; do
    sbcl --noinform --non-interactive --load run-tests.lisp
done
