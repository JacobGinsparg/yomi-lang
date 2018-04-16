#lang racket

(require "../../lib/yomi-lang.rkt")
(using-game "schema-skullgirls.rkt")


; Filia character schema
(define-character filia
  ; normals
  
  (move LP 5 3 7 14)
  (move MP 13 3 9 19)
  (move HP 12 3 11 21)
  (move LK 9 3 7 19)
  (move MK 10 3 9 17)
  (move HK 19 9 14 26)
  (move 2LP 6 3 7 7)
  (move 2MP 13 3 9 38)
  (move 2HP 12 3 9 32)
  (move 2LK 7 3 7 10)
  (move 2MK 10 18 25 24)  ; multi hit, possibly inaccurate
  (move 2HK 12 26 11 19)
  (move j.LP 7 4 7 7)
  (move j.MP 11 12 29 9)  ; multi hit, possibly inaccurate
  (move j.HP 10 8 11 15)
  (move j.LK 7 4 7 8)
  (move j.MK 11 15 14 19)  ; multi hit, possibly inaccurate
  (move j.HK 13 2 13 26)
  
  ; throws, no idea what durations are
  
  (move LP+LK 7 1 0 28)
  (move j.LP+LK 7 3 0 13)
  
  ; specials
  
  ; ringlet spike; possibly inaccurate
  (move 236LP 28 18 28 35)
  (move 236MP 28 18 28 35)
  (move 236HP 28 18 28 35)
  ; ringlet psych
  (move 236LK 18 0 0 10)
  ; updo, possibly inaccurate became m/h are multi hit
  (move 623LP 8 8 9 69)
  (move 623MP 10 10 15 69)
  (move 623HP 13 14 29 74)
  ; hairball
  (move 214LK 14 14 22 29)
  (move 214MK 14 20 24 29)
  (move 214HK 14 32 28 29)
  ; airball
  (move j.214LK 13 20 25 16)
  (move j.214MK 13 20 25 16)
  (move j.214HK 13 20 25 16)
  
  ; supers, these are likely inaccurate
  
  ; fenrir drive
  (move 623MP+HP 4 34 35 74)
  ; gregor samson (different properties in air)
  (move 214MK+HK 8 13 41 49)
  (move j.214MK+HK 7 34 44 0)  ; variable recovery based on when she lands
  ; tricobezoar
  (move 214MP+HP 8 2 19 100) ; is recovery accurate?

  ; aliases
  
  (alias fenrir 623MP+HP)
  (alias gregor 214MK+HK)
  (alias tricobezoar 214MP+HP)
  (alias throw LP+LK))