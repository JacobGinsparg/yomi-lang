#lang racket

(provide define-game
         buttons
         tick-rate
         move)

(require "./yomi-lib.rkt"
         (for-syntax syntax/parse
                     racket))

(define buttons   0)
(define tick-rate 1)

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
     #:with r (datum->syntax #'name 'racket)
     #:with req (datum->syntax #'name 'require)
     (define btn-symbols (map syntax->datum (syntax->list #'(btn ...))))
     (validate-buttons btn-symbols)
     (define tr-id (datum->syntax #'name 'TICK-RATE))
     #`(module name r
         (req racket)
         (provide #,tr-id (prefix-out button: btn) ...)
         (define buttons-remaining '(b1 b2 b3 b4 b5 b6 b7 b8))
         (define (allocate-button)
           (if (empty? buttons-remaining)
               (error 'allocate-button "no more buttons")
               (let ([next-button (first buttons-remaining)])
                 (set! buttons-remaining (rest buttons-remaining))
                 next-button)))
         (define #,tr-id tix)
         (define btn (allocate-button)) ...)]))

;; validate-buttons : [Listof Symbol] -> Void
(define-for-syntax (validate-buttons btn-list)
  (let ([seen (mutable-set)])
    (for-each (lambda (btn)
                (if (set-member? seen btn)
                    (error 'define-game "Duplicate button declared: ~s" btn)
                    (set-add! seen btn)))
              btn-list)
    (when (> (length btn-list) button-length)
      (error 'define-game "Can only define ~s buttons, ~s declared" button-length (length btn-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CHARACTER SCHEMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define-syntax define-character
  (syntax-parser
    [(_ name:id game:id
        )]))

(define-syntax move
  (syntax-parser
    [(_ mv:id startup:nat active:nat hitstun:nat recovery:nat)
     (define move-lambda (make-move-thunk (symbol->string (syntax->datum #'mv))))
     (define move-lambda-scoped (datum->syntax #'mv move-lambda))
     #`(define mv (make-move #,move-lambda-scoped startup active hitstun recovery))]))

;; make-move-thunk : String -> Sexpr
(define-for-syntax (make-move-thunk move-str)
  (let ([parsed-move (regexp-match move-regex move-str)])
    (unless parsed-move
      (error 'define-character "Bad move format"))
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
(define-for-syntax (explode-directions dir-string)
  (define (build-dir-list dir)
    (map (lambda (d) `(quote ,d)) (hash-ref direction-table dir)))
  (if (not dir-string)
      '()
      (map build-dir-list (regexp-match* dir-regex dir-string))))

;; explode-buttons : String -> [Listof Symbol]
(define-for-syntax (explode-buttons button-string)
  (map (lambda (b) (string->symbol (string-append "button:" b)))
       (string-split button-string "+")))

;; build-press : [Listof Symbol] -> Sexpr
(define-for-syntax (build-press cmds)
  `(press ,@cmds))