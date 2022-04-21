#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "tokenizer/category.rkt")

(require digimon/character)
(require digimon/stdio)

(require racket/string)
(require racket/unsafe/ops)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct c-srcloc
  ([in : Input-Port]
   [source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name C-Srcloc)

(define-syntax (c-make-token stx)
  (syntax-case stx []
    [(_ src make-c:token datum ...)
     (syntax/loc stx
       (let-values ([(line column here-position) (syn-token-port-location (c-srcloc-in src))])
         (make-c:token (c-srcloc-source src) (c-srcloc-line src) (c-srcloc-column src)
                         (c-srcloc-position src) here-position datum ...)))]))
  
(define-syntax (c-make-bad-token stx)
  (syntax-case stx []
    [(_ src c:bad:sub token datum)
     (syntax/loc stx
       (let ([bad (c-make-token src c:bad:sub (~s (cons (object-name token) datum)))])
         (c-log-read-error (c-token->string bad))
         bad))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-token : (-> Input-Port (U String Symbol) Boolean (U C-Token EOF))
  (lambda [/dev/cin source cpp?]
    (define-values (line column position) (syn-token-port-location /dev/cin))
    (define srcloc (c-srcloc /dev/cin source line column position))
    (define ch (read-char /dev/cin))
    (cond [(eof-object? ch) eof]
          [(char-whitespace? ch) (c-consume-whitespace-token srcloc)]
          ;[(char-numeric? ch) (c-consume-numeric-token srcloc ch #false)]
          [(c-identifier-initial-char? ch) (c-consume-identifier-token srcloc ch cpp?)]
          [else (case ch
                  [(#\( #\[ #\{) (c-make-token srcloc c:open ch)]
                  [(#\) #\] #\}) (c-make-token srcloc c:close ch)]
                  ;[(#\") (c-consume-string-token srcloc ch)]
                  ;[(#\+) (c-consume-numeric-token srcloc ch #true)]
                  ;[(#\.) (c-consume-numeric-token srcloc ch #false)]
                  ;[(#\^ #\$ #\| #\~ #\*) (c-consume-match-token srcloc ch)]
                  ;[(#\#) (c-consume-hash-token srcloc)]
                  [(#\/) (c-consume-comment-token srcloc)]
                  [(#\;) (c-make-token srcloc c:semicolon #\;)]
                  [(#\,) (c-make-token srcloc c:comma #\,)]
                  [(#\=) (c-make-token srcloc c:eq #\=)]
                  [(#\:) (c-make-token srcloc c:colon #\:)]
                  [(#\\) (c-consume-universal-identifier-token srcloc cpp?)]
                  [else (c-make-bad-token srcloc c:bad:char struct:c:punctuator ch)])])))

(define c-consume-comment-token : (-> C-Srcloc (U C:WhiteSpace C:Operator C:Bad))
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (cond [(eof-object? ch1) (c-make-token srcloc c:slash #\/)]
          [(char=? ch1 #\/) (read-char /dev/cin) (let ([c (read-line /dev/cin)]) (c-make-token srcloc c:whitespace (if (string? c) c "")))]
          [(not (char=? ch1 #\*)) (c-make-token srcloc c:slash #\/)]
          [(regexp-match #px".*?((\\*/)|$)" /dev/cin) => (λ [**/] (c-make-token srcloc c:whitespace (format "/~a" (car **/))))]
          [else (c-make-bad-token srcloc c:bad:eof struct:c:whitespace "/*")])))

(define c-consume-whitespace-token : (-> C-Srcloc C:WhiteSpace)
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (c-consume-whitespace /dev/cin)
    (c-make-token srcloc c:whitespace #\space)))
  
(define c-consume-identifier-token : (-> C-Srcloc Char Boolean (U C:Identifier C:Keyword C:String C:Bad))
  (lambda [srcloc leader cpp?]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define maybe-id : (U String (Listof Char)) (c-consume-ascii-identifier /dev/cin leader))
    
    (cond [(list? maybe-id) (c-make-bad-token srcloc c:bad:char struct:c:identifier (list->string maybe-id))]
          [else (let-values ([(keywords keyword-map) (if (not cpp?) (values c-keywords c-keyword-map) (values cpp-keywords cpp-keyword-map))]
                             [(sym) (string->symbol maybe-id)])
                  (cond [(memq sym keywords) (c-make-token srcloc c:keyword (hash-ref keyword-map sym))]
                        [else (c-make-token srcloc c:identifier sym)]))])))
  
(define c-consume-universal-identifier-token : (-> C-Srcloc Boolean (U C:Identifier C:Keyword C:String C:Bad))
  (lambda [srcloc cpp?]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define maybe-head : (U Char (Listof Char)) (c-consume-universal-char /dev/cin #false 0))

    (cond [(list? maybe-head) (c-make-bad-token srcloc c:bad:char struct:c:identifier (list->string maybe-head))]
          [else (c-consume-identifier-token srcloc maybe-head cpp?)])))

#;(define c-consume-string-token : (-> C-Srcloc Char (U C:String C:Bad))
  (lambda [srcloc quotation]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (let consume-string-token : (U C:String C:Bad) ([chars : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/cin))
      (cond [(or (eof-object? ch) (char=? ch quotation))
             (when (eof-object? ch) (c-make-bad-token srcloc c:bad:eof struct:c:string (list->string (reverse chars))))
             (c-make-token srcloc c:string (list->string (reverse chars)))]
            [(char=? ch #\newline) (c-make-bad-token srcloc c:bad:eol struct:c:string (list->string (reverse chars)))]
            [(not (char=? ch #\\)) (consume-string-token (cons ch chars))]
            [else (let ([next (peek-char /dev/cin)])
                    (cond [(eof-object? next) (consume-string-token chars)]
                          [(and (char=? next #\newline) (read-char /dev/cin)) (consume-string-token (cons ch chars))]
                          [else (consume-string-token (cons (c-consume-universal-char /dev/cin) chars))]))]))))

#;(define c-consume-numeric-token : (-> C-Srcloc Char Boolean (U C-Numeric C:Delim C:Bad))
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  ;;; https://drafts.csswg.org/c-values/#numeric-types
  (lambda [srcloc sign/digit signed?]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (define ch2 : (U EOF Char) (peek-char /dev/cin 1))
    (cond [(not (c-number-prefix? sign/digit ch1 ch2)) (c-make-token srcloc c:delim sign/digit)]
          [else (let-values ([(n representation) (c-consume-number /dev/cin sign/digit)])
                  (let ([ch1 : (U EOF Char) (peek-char /dev/cin 0)]
                        [ch2 : (U EOF Char) (peek-char /dev/cin 1)]
                        [ch3 : (U EOF Char) (peek-char /dev/cin 2)])
                    (cond [(c-identifier-prefix? ch1 ch2 ch3)
                           (define unit : Symbol (string->symbol (string-downcase (c-consume-name /dev/cin #false))))
                           (define value : Flonum (real->double-flonum n))
                           (define rep+unit : String (~a representation unit))
                           (case unit
                             [(em ex ch ic rem)       (c-make-token srcloc c:length:font     rep+unit signed? value unit)]
                             [(cap lh rlh)            (c-make-token srcloc c:length:font     rep+unit signed? value unit)]
                             [(vw vh vi vb vmin vmax) (c-make-token srcloc c:length:viewport rep+unit signed? value unit)]
                             [(px cm mm q in pc pt)   (c-make-token srcloc c:length          rep+unit signed? value unit)]
                             [(apc pls ls)            (c-make-token srcloc c:length          rep+unit signed? value unit)]
                             [(deg grad rad turn)     (c-make-token srcloc c:angle           rep+unit signed? value unit)]
                             [(s ms min h mtn tn)     (c-make-token srcloc c:time            rep+unit signed? value unit)]
                             [(hz khz)                (c-make-token srcloc c:frequency       rep+unit signed? value unit)]
                             [(dpi dpcm dppx x)       (c-make-token srcloc c:resolution      rep+unit signed? value unit)]
                             [else                    (c-make-token srcloc c:dimension       rep+unit signed? value unit)])]
                          [(and (char? ch1) (char=? ch1 #\%) (read-char /dev/cin))
                           (define n% : Flonum (real->double-flonum (* n 0.01)))
                           (c-make-token srcloc c:percentage (string-append representation "%") signed? n%)]
                          [(flonum? n)
                           (cond [(zero? n) (c-make-token srcloc c:flzero representation signed? n)]
                                 [(fl= n 1.0) (c-make-token srcloc c:flone representation signed? n)]
                                 [else (c-make-token srcloc c:flonum representation signed? n)])]
                          [(zero? n) (c-make-token srcloc c:zero representation signed? n)]
                          [(= n 1) (c-make-token srcloc c:one representation signed? n)]
                          [else (c-make-token srcloc c:integer representation signed? n)])))])))

#;(define c-consume-hash-token : (-> C-Srcloc (U C:Hash C:Delim))
  ;;; https://drafts.csswg.org/c-syntax/#hash-token-diagram
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (define ch2 : (U EOF Char) (peek-char /dev/cin 1))
    (define ch3 : (U EOF Char) (peek-char /dev/cin 2))
    (if (or (c-char-name? ch1) (c-valid-escape? ch1 ch2))
        (let ([name (c-consume-name (c-srcloc-in srcloc) #false)])
          (c-make-token srcloc c:hash (string->keyword name) #|(string->keyword (string-downcase name))|#))
        (c-make-token srcloc c:delim #\#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-whitespace : (-> Input-Port Void)
  (lambda [/dev/cin]
    (regexp-match #px"\\s*" /dev/cin)
    (void)))
  
(define c-consume-ascii-identifier : (-> Input-Port (Option Char) (U String (Listof Char)))
  (lambda [/dev/cin ?leader]
    (let consume-ascii-id ([span : Nonnegative-Fixnum 0]
                           [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/cin skip))
      (cond [(eof-object? ch) (read-tail-string /dev/cin span ?leader)]
            [(c-identifier-char? ch) (consume-ascii-id (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
            [(eq? ch #\\) (c-consume-unicode-identifier /dev/cin (read-tail-string /dev/cin span ?leader))]
            [else (read-tail-string /dev/cin span ?leader)]))))

(define c-consume-unicode-identifier : (-> Input-Port (Option String) (U String (Listof Char)))
  (lambda [/dev/cin ?head]
    (let consume-unicode-id ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (peek-char /dev/cin))
      (cond [(eof-object? ch) (string-append (or ?head "") (list->string (reverse srahc)))]
            [(c-identifier-char? ch) (consume-unicode-id (cons ch srahc))]
            [(not (eq? ch #\\)) (string-append (or ?head "") (list->string (reverse srahc)))]
            [else (let ([maybe-unicode (c-consume-universal-char /dev/cin ?head 1)])
                    (cond [(char? maybe-unicode) (consume-unicode-id (cons maybe-unicode srahc))]
                          [else maybe-unicode]))]))))

#;(define c-consume-number : (-> Input-Port Char (Values (U Flonum Integer) String))
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [/dev/cin sign/digit]
    (let consume-number ([chars (list sign/digit)])
      (define ch : (U EOF Char) (peek-char /dev/cin))
      (cond [(and (char? ch)
                  (or (char-numeric? ch)
                      (char=? ch #\+) (char=? ch #\-)
                      (c-decimal-point? ch (peek-char /dev/cin 1))
                      (c-scientific-notation? ch (peek-char /dev/cin 1) (peek-char /dev/cin 2)))
                  (read-char /dev/cin))
             (consume-number (cons ch chars))]
            [else (let* ([representation : String (list->string (reverse chars))]
                         [?number : (Option Complex) (string->number representation)])
                    (cond [(exact-integer? ?number) (values ?number representation)]
                          [(flonum? ?number) (values ?number representation)]
                          [else (values +nan.0 representation)]))]))))

(define c-consume-universal-char : (-> Input-Port (Option String) (U One Zero) (U Char (Listof Char)))
  (lambda [/dev/cin ?head skip0]
    (define initial? : Boolean (not (non-empty-string? ?head)))
    (define shortform-type : (U EOF Char) (peek-char /dev/cin skip0))
    (define shortform-size : (Option Byte) (case shortform-type [(#\u) 4] [(#\U) 8] [else #false]))
    
    (cond [(not shortform-size) (c-consume-error-chars/word /dev/cin ?head)]
          [else (let-values ([(unicode count) (peek-unicode-char /dev/cin (+ skip0 1) 0 0)])
                  (cond [(not (= count shortform-size)) (c-consume-error-chars/word /dev/cin ?head)]
                        [(not (c-identifier-universal-char? unicode)) (c-consume-error-chars/word /dev/cin ?head)]
                        [(and initial? (c-identifier-non-initial-char? unicode)) (c-consume-error-chars/word /dev/cin ?head)]
                        [else (drop-bytes /dev/cin (+ 1 skip0 count)) unicode]))])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define c-char-non-printable? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char<=? #\null ch #\backspace)
             (char=? ch #\vtab)
             (char<=? #\u000E ch #\u001F)
             (char=? ch #\rubout)))))

(define c-identifier-char? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-alphabetic? ch)
             (char-numeric? ch)
             (char=? #\_ ch)
             (c-identifier-implementation-defined-char? ch)))))

(define c-identifier-initial-char? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-alphabetic? ch)
             (char=? #\_ ch)
             (c-identifier-implementation-defined-char? ch)))))

(define c-identifier-implementation-defined-char? : (-> Char Boolean)
  ; these chars as identifiers' prefixes are borrowed from CSS
  (lambda [ch]
    (or (char=? #\u00B7 ch)
        (char<=? #\u00C0 ch #\u00D6)
        (char<=? #\u00D8 ch #\u00F6)
        (char<=? #\u00F8 ch #\u037D)
        (char<=? #\u037F ch #\u1FFF)
        (char=? #\u200C ch)
        (char=? #\u200D ch)
        (char=? #\u203F ch)
        (char=? #\u2040 ch)
        (char<=? #\u2070 ch #\u218F)
        (char<=? #\u2C00 ch #\u2FEF)
        (char<=? #\u3001 ch #\uD7FF)
        (char<=? #\uF900 ch #\uFDCF)
        (char<=? #\uFDF0 ch #\uFFFD)
        (char>=? ch #\U10000))))

(define c-identifier-universal-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char=? #\u00A8 ch)
        (char=? #\u00AA ch)
        (char=? #\u00AD ch)
        (char=? #\u00AF ch)
        (char<=? #\u00B2 ch #\u00B5)
        (char<=? #\u00B7 ch #\u00BA)
        (char<=? #\u00BC ch #\u00BE)
        (char<=? #\u00C0 ch #\u00D6)
        (char<=? #\u00D8 ch #\u00F6)
        (char<=? #\u00F8 ch #\u00FF)
        (char<=? #\u0100 ch #\u167F)
        (char<=? #\u1681 ch #\u180D)
        (char<=? #\u180F ch #\u1FFF)
        (char<=? #\u200B ch #\u200D)
        (char<=? #\u202A ch #\u202E)
        (char<=? #\u203F ch #\u2040)
        (char=? #\u2054 ch)
        (char<=? #\u2060 ch #\u206F)
        (char<=? #\u2070 ch #\u218F)
        (char<=? #\u2460 ch #\u24FF)
        (char<=? #\u2776 ch #\u2793)
        (char<=? #\u2E80 ch #\u2FFF)
        (char<=? #\u3004 ch #\u3007)
        (char<=? #\u3021 ch #\u302F)
        (char<=? #\u3031 ch #\uD7FF)
        (char<=? #\uF900 ch #\uFD3D)
        (char<=? #\uFD40 ch #\uFDCF)
        (char<=? #\uFDF0 ch #\uFE44)
        (char<=? #\uFE47 ch #\uFFFD)
        (char<=? #\U10000 ch #\U1FFFD)
        (char<=? #\U20000 ch #\U2FFFD)
        (char<=? #\U30000 ch #\U3FFFD)
        (char<=? #\U40000 ch #\U4FFFD)
        (char<=? #\U50000 ch #\U5FFFD)
        (char<=? #\U60000 ch #\U6FFFD)
        (char<=? #\U70000 ch #\U7FFFD)
        (char<=? #\U80000 ch #\U8FFFD)
        (char<=? #\U90000 ch #\U9FFFD)
        (char<=? #\UA0000 ch #\UAFFFD)
        (char<=? #\UB0000 ch #\UBFFFD)
        (char<=? #\UC0000 ch #\UCFFFD)
        (char<=? #\UD0000 ch #\UDFFFD)
        (char<=? #\UE0000 ch #\UEFFFD))))

(define c-identifier-non-initial-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char<=? #\u0300 ch #\u036F)
        (char<=? #\u1DC0 ch #\u1DFF)
        (char<=? #\u20D0 ch #\u20FF)
        (char<=? #\uFE20 ch #\uFE2F))))
  
#;(define c-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#escaping
  ;;; https://drafts.csswg.org/c-syntax/#starts-with-a-valid-escape
  (lambda [ch1 ch2]
    (and (char? ch1)
         (char=? ch1 #\\)
         (or (eof-object? ch2)
             (not (char=? ch2 #\newline))))))

#;(define c-identifier-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#would-start-an-identifier
  (lambda [ch1 ch2 ch3]
    (or (c-char-name-prefix? ch1)
        (c-valid-escape? ch1 ch2)
        (and (char? ch1) (char=? ch1 #\-)
             (or (c-char-name-prefix? ch2)
                 (and (char? ch2) (char=? ch2 #\-))
                 (c-valid-escape? ch2 ch3))))))

#;(define c-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#starts-with-a-number
  (lambda [ch1 ch2 ch3]
    (or (and (char? ch1) (char<=? #\0 ch1 #\9))
        (and (char? ch1) (char? ch2)
             (char=? ch1 #\.) (char<=? #\0 ch2 #\9))
        (and (char? ch1) (char? ch2)
             (or (char=? ch1 #\+) (char=? ch1 #\-))
             (or (char<=? #\0 ch2 #\9)
                 (and (char=? ch2 #\.)
                      (char? ch3) (char<=? #\0 ch3 #\9)))))))

#;(define c-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [ch1 ch2 ch3]
    (and (char? ch1) (char? ch2)
         (char-ci=? ch1 #\e)
         (or (char<=? #\0 ch2 #\9)
             (and (or (char=? ch2 #\+) (char=? ch2 #\-))
                  (char? ch3)
                  (char<=? #\0 ch3 #\9))))))

#;(define c-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [ch1 ch2]
    (and (char? ch1) (char? ch2)
         (char=? ch1 #\.)
         (char<=? #\0 ch2 #\9))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-error-chars/word : (-> Input-Port (Option String) (Listof Char))
  (lambda [/dev/cin ?head]
    (let consume-word ([srahc : (Listof Char) (if (not ?head) null (reverse (string->list ?head)))])
      (define ch : (U EOF Char) (read-char /dev/cin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(not (char-whitespace? ch)) (consume-word (cons ch srahc))]
            [else (reverse srahc)]))))
