#lang racket

(provide define-game
         buttons
         tick-rate
         move)

(require (for-syntax syntax/parse
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
         (provide #,tr-id btn ...)
         (define buttons-remaining '(b1 b2 b3 b4 b5 b6 b7 b8))
         (define (allocate-button)
           (if (empty? buttons-remaining)
               (error 'allocate-button "no more buttons")
               (let ([next-button (first buttons-remaining)])
                 (set! buttons-remaining (rest buttons-remaining))
                 next-button)))
         (define #,tr-id tix)
         (define btn (allocate-button)) ...)]))

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
    (let ([prefix (second parsed-move)]
          [directions (third parsed-move)]
          [buttons (fourth parsed-move)])
      `(lambda () ,@(build-direction-commands directions) ,@(build-button-commands buttons)))))

;; build-direction-commands : String -> [Listof Sexpr]
(define-for-syntax (build-direction-commands dir-string)
  (define (build-dir-execution dir)
    `(press @,(hash-ref direction-table dir)))
  (map build-dir-execution (regexp-match* dir-regex dir-string)))

;; build-button-commands : String -> [Listof Sexpr]
(define-for-syntax (build-button-commands button-string)
  (list `(press @,(map string->symbol (string-split button-string "+")))))

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