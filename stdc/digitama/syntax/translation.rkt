#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "tokenizer.rkt")
(require "stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct c-source
  ([contents : (Listof C-Token)])
  #:transparent
  #:type-name C-Source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-c-source : (-> Syn-Token-Stdin C-Source)
  (lambda [/dev/rawin]
    (define /dev/cin : Input-Port (cpp-open-input-port /dev/rawin #false))
    (define source : (U String Symbol) (syn-token-port-name /dev/cin))
    (define tokens : (Listof C-Token) (c-consume-tokens /dev/cin source))
    
    (c-source tokens)))
