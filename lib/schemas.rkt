#lang racket

(provide define-game
         define-character
         define-combo
         using-game
         using-character
         buttons
         tick-rate
         move
         ~ &)

(require "./yomi-lib.rkt"
         (for-syntax syntax/parse
                     syntax/stx
                     racket/syntax
                     (except-in racket
                                link)
                     "./yomi-lib.rkt"))

(define buttons   0)
(define tick-rate 1)
(define & 2)
(define ~ 3)

(define-for-syntax tick-rate-id 'TICK-RATE)

(define-for-syntax game-defined? #f)
(define-for-syntax (flip-game-defined)
  (set! game-defined? #t))
(define-for-syntax game-loaded? #f)
(define-for-syntax (set-game-loaded g)
  (set! game-loaded? g))
(define-for-syntax character-defined? #f)
(define-for-syntax (flip-character-defined)
  (set! character-defined? #t))
(define-for-syntax character-loaded? #f)
(define-for-syntax (set-character-loaded c)
  (set! character-loaded? c))

(define-for-syntax move-regex
  (regexp "^([a-z]\\.)?((?:[1-9](?:<[0-9]+>)?)+)?([A-z]+(?:\\+[A-z]+)*(?:<[0-9]+>)?)$"))
(define-for-syntax dir-regex
  (regexp "[1-9](?:<[0-9]+>)?"))

(define-for-syntax direction-table
  (hash
   "1" '(down back)
   "2" '(down)
   "3" '(down forward)
   "4" '(back)
   "5" '()
   "6" '(forward)
   "7" '(up back)
   "8" '(up)
   "9" '(up forward)))

;; Length of the buttons-remaining list,
;; hardcoded due to phasing conflict
(define-for-syntax button-length 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME SCHEMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-game
  (syntax-parser
    #:literals (buttons tick-rate)
    [(_ name:id
        [buttons btn:id ...]
        [tick-rate tix])
     #:with (prefixed-btn ...) (stx-map prefix-button-id #'(btn ...))
     (define btn-list (syntax->list #'(btn ...)))
     (validate-buttons btn-list)
     (define tr-id (datum->syntax #'name tick-rate-id))
     #`(begin
         (begin-for-syntax
           (if game-defined?
               (error 'game "Game schema already defined")
               (flip-game-defined)))
         (provide #,tr-id prefixed-btn ...)
         (define buttons-remaining '(b1 b2 b3 b4 b5 b6 b7 b8))
         (define (allocate-button)
           (if (empty? buttons-remaining)
               (error 'allocate-button "no more buttons")
               (let ([next-button (first buttons-remaining)])
                 (set! buttons-remaining (rest buttons-remaining))
                 next-button)))
         (define #,tr-id tix)
         (define prefixed-btn (allocate-button)) ...)]
    [stx
     (raise-syntax-error 'define-game "Bad game schema" #'stx)]))

(define-syntax using-game
  (syntax-parser
    [(_ path:string)
     #`(begin
         (begin-for-syntax
           (when game-loaded?
             (error 'game "Already using game schema: ~a" game-loaded?)))
         (require path)
         (begin-for-syntax
           (unless game-defined?
             (error 'game "Game schema not defined in: ~a" path))
           (set-game-loaded path)))]))

;; validate-buttons : [Listof Syntax] -> Void
;; Checks if there are any duplicate buttons and
;; if there are no more than 8 buttons declared
;; SIDE EFFECTS:
;; - error if dups
;; - error if too many buttons
(define-for-syntax (validate-buttons btn-stx-list)
  (let ([seen (mutable-set)])
    (for-each (lambda (btn)
                (let ([btn-sym (syntax->datum btn)])
                  (if (set-member? seen btn-sym)
                      (wrong-syntax btn "Duplicate button declared")
                      (set-add! seen btn-sym))))
              btn-stx-list)
    (when (> (length btn-stx-list) button-length)
      (wrong-syntax (list-ref btn-stx-list button-length) "Excess button"))))

;; prefix-button-id : Id -> Id
;; Prefixes the given identifier with "button:"
(define-for-syntax (prefix-button-id button)
  (let [(button-name-string (symbol->string (syntax->datum button)))]
    (datum->syntax button
                   (string->symbol (string-append "button:"
                                                  button-name-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CHARACTER SCHEMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-character
  (syntax-parser
    [(_ name:id (~and full-move ((~literal move) move-name move-exprs ...)) ...
                ((~literal alias) alias-name aliased-move) ...)
     (define tr-id (datum->syntax #'name tick-rate-id))
     (define move-names-set (list->set (stx-map syntax->datum #'(move-name ...))))
     (for-each (lambda (a)
                 (let ([a-data (syntax->datum a)])
                   (unless (set-member? move-names-set a-data)
                     (raise-syntax-error 'alias "Move not defined" a))))
               (syntax->list #'(aliased-move ...)))
     #`(begin
         (begin-for-syntax
           (if character-defined?
               (error 'character "Character schema already defined")
               (flip-character-defined)))
         (provide #,tr-id move-name ... alias-name ...)
         full-move ...
         (define alias-name aliased-move) ...)]
    [stx
     (raise-syntax-error 'define-character "Bad character schema" #'stx)]))

(define-syntax using-character
  (syntax-parser
    [(_ path:string)
     #`(begin
         (begin-for-syntax
           (when character-loaded?
             (error 'character "Already using character schema: ~a" character-loaded?)))
         (require path)
         (begin-for-syntax
           (unless character-defined?
             (error 'character "Character schema not defined in: ~a" path))
           (set-character-loaded path)))]))

(define-syntax move
  (syntax-parser
    [(_ mv:id startup:nat active:nat hitstun:nat recovery:nat)
     (define move-lambda (make-move-thunk #'mv))
     (define move-lambda-scoped (datum->syntax #'mv move-lambda))
     #`(define mv (make-move #,move-lambda-scoped startup active hitstun recovery))]
    [stx
     (raise-syntax-error 'move "Bad move syntax" #'stx)]))

;; make-move-thunk : String -> Sexpr
;; Takes a move's string form and produces a thunk with the proper presses/holds
;; SIDE EFFECTS:
;; - errors if move string is bad
(define-for-syntax (make-move-thunk move-stx)
  (let ([parsed-move (regexp-match move-regex (symbol->string (syntax->datum move-stx)))])
    (unless parsed-move
      (wrong-syntax move-stx "Incorrect move format"))
    (match-define (list _ prefix directions buttons) parsed-move)
    (let ([directions (explode-directions directions)]
          [buttons (explode-buttons buttons)])
      (cond [(empty? directions) `(lambda () ,(build-press buttons))]
            [else
             (begin (define-values (first-directions last-direction) (split-at-right directions 1))
                    `(lambda ()
                       ,@(map build-press first-directions)
                       ,(build-press (append (first last-direction) buttons))))]))))

;; explode-directions : String -> [Listof [Listof Symbol]]
;; Takes directions in string form and produces a list of their symbol representation
(define-for-syntax (explode-directions dir-string)
  (define (build-dir-list dir)
    (map (lambda (d) `(quote ,d)) (hash-ref direction-table dir)))
  (if (not dir-string)
      '()
      (map build-dir-list (regexp-match* dir-regex dir-string))))

;; explode-buttons : String -> [Listof Symbol]
;; Takes buttons in string form and produces a list of their symbol representation
(define-for-syntax (explode-buttons button-string)
  (map (lambda (b) (string->symbol (string-append "button:" b)))
       (string-split button-string "+")))

;; build-press : [Listof Symbol] -> Sexpr
;; Takes a list of directions and/or buttons and produces a press
(define-for-syntax (build-press cmds)
  `(press ,@cmds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMBOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-combo
  (syntax-parser
    [(_ name:id (~seq move-id:id chainer:id) ... last-move-id:id)
     (validate-chainers (syntax->list #'(chainer ...)))
     (define move-chain (datum->syntax #'name (make-move-chain (syntax->list #'((move-id chainer) ...)))))
     #`(define name (list #,@move-chain last-move-id))]
    [stx
     (raise-syntax-error 'define-combo "Malformed combo" #'stx)]))

;; make-move-chain : [Listof Syntax] -> Sexpr
;; Takes a list of move links/cancels and produces a list of their combo form
(define-for-syntax (make-move-chain moves)
  `(,@(append-map (lambda (mv)
                    (let* ([mv-data (syntax->datum mv)]
                           [move-name (first mv-data)]
                           [chainer (if (symbol=? (second mv-data) '~) 'cancel 'link)])
                      `(,move-name (,chainer ,move-name TICK-RATE))))
                  moves)))

;; validate-chainers : [Listof Syntax] -> Void
;; Validates that all of the given syntax objects are ~ or & identifiers
(define-for-syntax (validate-chainers chainers)
  (for-each (lambda (ch)
              (let ([ch-sym (syntax->datum ch)])
                (unless (or (symbol=? '~ ch-sym) (symbol=? '& ch-sym))
                  (wrong-syntax ch "Use ~~ for cancels, & for links"))))
            chainers))