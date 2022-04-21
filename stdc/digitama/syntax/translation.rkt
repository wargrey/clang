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
(define read-c-source : (-> Syn-Token-StdIn C-Source)
  (lambda [/dev/rawin]
    (define /dev/cin : Input-Port (cpp-open-input-port /dev/rawin #false))
    (define source : (U String Symbol) (syn-token-port-name /dev/cin))

    (define tokens : (Listof C-Token)
      (let read-c-token ([snekot : (Listof C-Token) null])
        (define t (c-consume-token /dev/cin source #false))
        
        (cond [(eof-object? t) (reverse snekot)]
              [else (read-c-token (cons t snekot))])))

    (c-source tokens)))
