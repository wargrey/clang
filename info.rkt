#lang info

(define collection 'multi)
(define pkg-desc "Toolbox for C family in pure Typed Racket")
(define pkg-authors '(wargrey))

(define version "1.0")
(define test-omit-paths 'all)

(define deps '("digimon"
               "base" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

