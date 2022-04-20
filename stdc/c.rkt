#lang typed/racket/base

(provide (all-defined-out))
(provide read-c-source)

(require "digitama/syntax/translation.rkt")

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [c-read read]))
  (provide (rename-out [c-read-syntax read-syntax]))
  (provide (rename-out [c-info get-info]))
  
  (require stdc/village/clang/reader))
