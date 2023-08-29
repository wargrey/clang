#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/flonum racket/fixnum racket/list racket/format))
(provide (struct-out SYN-Token) syn-token-port-location syn-token-port-name syn-token-skip-whitespace)

(require racket/fixnum)
(require racket/flonum)
(require racket/list)
(require racket/format)
(require racket/symbol)
(require racket/keyword)
(require racket/match)

(require digimon/token)

(require (for-syntax racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/symbol))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-token stx)
  (syntax-parse stx #:literals [: Symbol Keyword]
    [(_ id : Number parent #:as Type #:=? type=? #:with id? id-datum rest ...)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))])
       (syntax/loc stx
         (begin (struct id parent ([datum : Type] rest ...) #:transparent #:type-name Number)
                (define (id=? [t1 : Number] [t2 : Number]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Number #:= type=? #:for C-Syntax-Any #:throw exn:c:range))))]
    [(_ id : Identifier parent ((~and (~or Symbol Keyword) Type) rest ...) #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))])
       (syntax/loc stx
         (begin (struct id parent ([datum : Type] rest ...) #:transparent #:type-name Identifier)
                (define (id=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Identifier #:eq? eq? #:for C-Syntax-Any #:throw exn:c:range))))]
    [(_ id : Otherwise parent (Type rest ...) #:with id? id-datum)
     (with-syntax ([type=? (case (syntax-e #'Type) [(String) #'string=?] [(Char) #'char=?] [else #'equal?])]
                   [id=? (format-id #'id "~a=?" (syntax-e #'id))])
       (syntax/loc stx
         (begin (struct id parent ([datum : Type] rest ...) #:transparent #:type-name Otherwise)
                (define (id=? [t1 : Otherwise] [t2 : Otherwise]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Otherwise #:eq? type=? #:for C-Syntax-Any #:throw exn:c:range))))]))

(define-syntax (define-symbolic-tokens stx)
  (syntax-parse stx
    [(_ token #:+ Token [id #:+ ID #:as Type rest ...] ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'token "~a-Datum" (syntax-e #'Token))]
                   [([id? id-datum] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a-datum" (syntax-e <id>))))])
       (syntax/loc stx
         (begin (struct token c-token () #:transparent #:type-name Token)
                (define-token id : ID token (Type rest ...) #:with id? id-datum) ...
                (define-type Token-Datum (U Type ...))
                (define (token->datum [t : Token]) : (Option Token-Datum) (cond [(id? t) (id-datum t)] ... [else #false])))))]))

(define-syntax (define-numeric-tokens stx)
  (syntax-case stx []
    [(_ token #:+ Token #:nan nan #:-> parent [id #:+ ID #:as Type rest ...] ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [([id? id=? id-datum type=?] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))]
                               [<type> (in-list (syntax->list #'(Type ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a=?" (syntax-e <id>))
                            (format-id <id> "~a-datum" (syntax-e <id>))
                            (let ([type-name (symbol->immutable-string (syntax-e <type>))])
                              (cond [(string-contains? type-name "Flonum") #'fl=]
                                    [(string-contains? type-name "Fixnum") #'fx=]
                                    [else #'=]))))])
       (syntax/loc stx
         (begin (struct token parent () #:transparent #:type-name Token)
                (define-token id : ID token #:as Type #:=? type=? #:with id? id-datum rest ...) ...
                (define (token->datum [t : Token]) : (U Type ...) (cond [(id? t) (id-datum t)] ... [else nan])))))]))
  
(define-syntax (define-tokens stx)
  (syntax-case stx []
    [(_ token header #:+ Token
        [[ptoken #:+ PToken #:-> pparent pfields] ...]
        [[ctoken #:+ CToken #:-> cparent] ...]
        (define-typical-tokens group #:+ Group rest ...) ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'Token "~a-Datum" (syntax-e #'Token))]
                   [([type? type->datum] ...)
                    (for/list ([<type> (in-list (syntax->list #'(group ...)))]
                               #:unless (eq? (syntax-e <type>) 'c:dimension))
                      (list (format-id <type> "~a?" (syntax-e <type>))
                            (format-id <type> "~a->datum" (syntax-e <type>))))]
                   [(Symbolic-Datum ...)
                    (for/list ([<define> (in-list (syntax->list #'(define-typical-tokens ...)))]
                               [<Type> (in-list (syntax->list #'(Group ...)))]
                               #:when (eq? (syntax-e <define>) 'define-symbolic-tokens))
                      (format-id <Type> "~a-Datum" (syntax-e <Type>)))])
       (syntax/loc stx
         (begin (struct token header () #:transparent #:type-name Token)
                (struct ptoken pparent pfields #:transparent #:type-name PToken) ...
                (define-typical-tokens group #:+ Group rest ...) ...
                (struct ctoken cparent () #:transparent #:type-name CToken) ...

                (define-type Token-Datum (U False Number (Pairof Number Symbol) Symbolic-Datum ...))
                (define token->datum : (-> Token Token-Datum)
                  (lambda [instance]
                    (cond [(type? instance) (type->datum instance)] ...
                          [else (assert (object-name instance) symbol?)]))))))]))

;;; https://drafts.csswg.org/c-syntax/#tokenization
;; https://drafts.csswg.org/c-syntax/#component-value
;; https://drafts.csswg.org/c-syntax/#current-input-token
(define-tokens c-token syn-token #:+ C-Token
  [[c-numeric         #:+ C-Numeric         #:-> c-token   ([representation : String] [suffix : (Option Symbol)])]]

  [[c:open            #:+ C:Open            #:-> c:punctuator]
   [c:colon           #:+ C:Colon           #:-> c:punctuator]
   [c:semicolon       #:+ C:Semicolon       #:-> c:punctuator]
   [c:comma           #:+ C:Comma           #:-> c:punctuator]
   [c:slash           #:+ C:Slash           #:-> c:punctuator]
   [c:eq              #:+ C:Eq              #:-> c:punctuator]
   
   [c:bad:eof         #:+ C:Bad:EOF         #:-> c:bad]
   [c:bad:eol         #:+ C:Bad:EOL         #:-> c:bad]
   [c:bad:raw         #:+ C:Bad:Raw         #:-> c:bad]
   [c:bad:char        #:+ C:Bad:Char        #:-> c:bad]
   [c:bad:blank       #:+ C:Bad:Blank       #:-> c:bad]
   [c:bad:range       #:+ C:Bad:Range       #:-> c:bad]
   [c:bad:suffix      #:+ C:Bad:Suffix      #:-> c:bad]]

  (define-symbolic-tokens c-symbolic-token #:+ C-Symbolic-Token
    [c:identifier     #:+ C:Identifier      #:as Symbol]
    [c:keyword        #:+ C:Keyword         #:as Keyword]
    [c:string         #:+ C:String          #:as String          [encoding : (Option Symbol)] [suffix : (Option Symbol)]]
    [c:char           #:+ C:Char            #:as Char            [encoding : (Option Symbol)] [suffix : (Option Symbol)]]
    [c:punctuator     #:+ C:Punctuator      #:as (U Char Symbol) [alternative : (Option (U Char Symbol))]]
    [c:whitespace     #:+ C:WhiteSpace      #:as (U String Char)])

  (define-numeric-tokens c-number #:+ C-Number #:nan +nan.0 #:-> c-numeric
    [c:multichar      #:+ C:MultiChar       #:as Integer  [encoding : (Option Symbol)]]
    [c:integer        #:+ C:Integer         #:as Fixnum]
    [c:flonum         #:+ C:Flonum          #:as Flonum])

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'. 
  (define-symbolic-tokens c-bad-token #:+ C-Bad-Token
    [c:bad            #:+ C:Bad             #:as (Pairof Symbol String)]
    [c:close          #:+ C:Close           #:as Char]))

(define-syntax-error exn:c #:as C-Syntax-Error #:for C-Token
  #:with [c-make-syntax-error c-log-syntax-error]
  [exn:c:resource           #:-> exn:c]
  [exn:c:deprecated         #:-> exn:c]
  [exn:c:loop               #:-> exn:c]
  [exn:c:namespace          #:-> exn:c]
  [exn:c:unrecognized       #:-> exn:c]
  [exn:c:misplaced          #:-> exn:c:unrecognized]
  [exn:c:type               #:-> exn:c:unrecognized]
  [exn:c:type:An+B          #:-> exn:c:type]
  [exn:c:type:identifier    #:-> exn:c:type]
  [exn:c:type:variable      #:-> exn:c:type:identifier]
  [exn:c:range              #:-> exn:c:unrecognized]
  [exn:c:unit               #:-> exn:c:range]
  [exn:c:digit              #:-> exn:c:range]
  [exn:c:overconsumption    #:-> exn:c:unrecognized]
  [exn:c:enclosed           #:-> exn:c:overconsumption]
  [exn:c:malformed          #:-> exn:c]
  [exn:c:arity              #:-> exn:c:malformed]
  [exn:c:empty              #:-> exn:c:malformed]
  [exn:c:missing-block      #:-> exn:c:malformed]
  [exn:c:missing-value      #:-> exn:c:malformed]
  [exn:c:missing-feature    #:-> exn:c:malformed]
  [exn:c:missing-keyword    #:-> exn:c:malformed]
  [exn:c:missing-delimiter  #:-> exn:c:malformed]
  [exn:c:missing-colon      #:-> exn:c:missing-delimiter]
  [exn:c:missing-comma      #:-> exn:c:missing-delimiter]
  [exn:c:missing-slash      #:-> exn:c:missing-delimiter])

(define c-signed-integer? : (-> C:Integer Boolean)
  (lambda [<B>]
    (or (< (c:integer-datum <B>) 0)
        (let ([raw (c-numeric-representation <B>)])
          (and (> (string-length raw) 0)
               (let ([sign (string-ref raw 0)])
                 (or (eq? sign #\+)
                     (eq? sign #\- #| -0 |#))))))))

(define c-token->syntax : (-> C-Token Syntax)
  (lambda [instance]
    (datum->syntax #false (c-token->datum instance)
                   (syn-token->syntax-location instance))))

(define c-token-datum->string : (-> C-Token String)
  (lambda [instance]
    (cond [(c:identifier? instance) (symbol->immutable-string (c:identifier-datum instance))]
          [(c-numeric? instance) (c-numeric-representation instance)]
          [(c:keyword? instance) (keyword->immutable-string (c:keyword-datum instance))]
          [(c:punctuator=:=? instance #\tab) "||"]
          [(c:string? instance) (c:string-datum instance)]
          [else (~a (c-token->datum instance))])))

(define c-token->string : (->* (C-Token) ((Option Any) (Option Any)) String)
  (lambda [instance [alt-object #false] [alt-datum #false]]
    (string-append (syn-token-location-string instance) ": "
                   (format "~a: ~a"
                     (or (object-name alt-object) (object-name instance))
                     (or alt-datum (c-token-datum->string instance))))))

(define #:forall (Error) c-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) Error)
                                                     (U C-Syntax-Any (Listof C-Token))
                                                     Error)
  (lambda [exn:css any]
    (match any
      [(or #false (list)) (exn:css (~a eof) (current-continuation-marks) null)]
      [(list token) (syn-token->exn exn:css c-token->string c-token->syntax token)]
      [(list main others ...) (syn-token->exn exn:css c-token->string c-token->syntax c-token-datum->string main (filter-not c:whitespace? others))]
      [(? c-token?) (syn-token->exn exn:css c-token->string c-token->syntax any)])))

(define c-log-syntax-error : (->* (C-Syntax-Error) ((Option C-Token) (Option Log-Level)) Void)
  (lambda [errobj [property #false] [level #false]]
    (syn-log-syntax-error 'exn:c:syntax c-token->string c-token->datum
                          errobj property (or level 'warning))))

(define c-log-read-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (syn-log-exn errobj 'exn:c:read src level)))

;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
(define-type C-Syntax-Any (Option C-Token))
(define-type (C-Multiplier idx) (U idx (List idx) (Pairof (U idx Symbol) (U idx Symbol))))
(define-type (C-Option css) (U css C-Syntax-Error False))
(define-type (C:Filter css) (-> C-Syntax-Any (C-Option css)))
(define-type (C-Parser css) (-> css (Listof C-Token) (Values (C-Option css) (Listof C-Token))))
