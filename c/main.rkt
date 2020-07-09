#lang typed/racket/base

(provide (all-defined-out))

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [c-read read]))
  (provide (rename-out [c-read-syntax read-syntax]))
  (provide (rename-out [c-info get-info]))
  
  (require c/village/clang/reader))
