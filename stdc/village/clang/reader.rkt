#lang racket/base

(provide (all-defined-out))
(provide (rename-out [c-read cpp-read]
                     [c-read-syntax cpp-read-syntax]
                     [c-read header-read]
                     [c-read-syntax header-read-syntax]))

(require racket/path)
(require racket/port)
(require racket/format)
(require racket/pretty)

(require syntax/strip-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define std-read-syntax
  (lambda [src /dev/cin modname read-c-document px.ext norm.ext]
    (regexp-match #px"^\\s*" /dev/cin) ; skip blanks before real c content
    (define-values (line column position) (port-next-location /dev/cin))
    (define bytes-bag (port->bytes /dev/cin))
    (define lang.c
      (cond [(path? src)
             (define src.c (path-replace-extension (file-name-from-path src) ""))
             (define path.c (if (regexp-match? px.ext src.c) src.c (path-replace-extension src.c norm.ext)))
             (string->symbol (path->string path.c))]
            [else '|this should not happen| 'lang.c]))
    (strip-context
     #`(module #,lang.c typed/racket/base
         (provide (all-from-out #,modname) #,lang.c)

         (require #,modname)

         (define-values (#,lang.c MB cpu real gc)
           (let ([/dev/rawin (open-input-bytes #,bytes-bag '#,src)]
                 [mem0 (current-memory-use)])
             (port-count-lines! /dev/rawin)
             (set-port-next-location! /dev/rawin #,line #,column #,position)
             (define-values (&lang.c cpu real gc) (time-apply #,read-c-document (list /dev/rawin)))
             (values (car &lang.c) (/ (- (current-memory-use) mem0) 1024.0 1024.0) cpu real gc)))

         (module+ main
           (require racket/pretty)
           (require racket/format)
           
           (pretty-print-columns 160)

           (define benchmark : String
             (format "[~a]memory: ~aMB cpu time: ~a real time: ~a gc time: ~a"
                     '#,lang.c (~r MB #:precision '(= 3)) cpu real gc))
           
           (define drracket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))
           (if drracket? #,lang.c (printf "~a~n~a~n" (pretty-format #,lang.c) benchmark))
           (when drracket? (displayln benchmark)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-read
  (lambda [[/dev/cin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cin) ; skip blanks between `#lang` and contents
    (port->bytes /dev/cin)))

(define c-read-syntax
  (lambda [[src #false] [/dev/cin (current-input-port)]]
    (std-read-syntax src /dev/cin 'stdc/c 'read-c-source #px"\\.c$" ".c")))

(define (c-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["C Sources" "*.c"])]
      [(drracket:default-extension) "c"]
      ;[(drracket:indentation) (dynamic-require 'sgml/village/clang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'stdc/village/clang/lexer 'c-lexer)]
      [else default])))

(define (cpp-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["C++ Sources" "*.cpp;*.cxx"])]
      [(drracket:default-extension) "cpp"]
      ;[(drracket:indentation) (dynamic-require 'sgml/village/clang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'stdc/village/clang/lexer 'cpp-lexer)]
      [else default])))

(define (header-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["Header Files" "*.h;*.hpp;*.hxx"])]
      [(drracket:default-extension) "h"]
      ;[(drracket:indentation) (dynamic-require 'sgml/village/clang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'stdc/village/clang/lexer 'c-lexer)]
      [else default])))
