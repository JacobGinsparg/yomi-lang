; Device that emulates a keyboard using the Linux kernel's uinput module.

#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(provide setup
         press
         hold
         release
         teardown)

; EventType and EventCode values are lifted from:
; https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h

; An EventType is an Integer
(define EV-KEY     #x1)
(define EV-SYN     #x0)

; An EventCode is an Integer
(define SYN-REPORT 0)
(define KEY-Q      16)
(define KEY-W      17)
(define KEY-E      18)
(define KEY-R      19)
(define KEY-T      20)
(define KEY-Y      21)
(define KEY-U      22)
(define KEY-I      23)
(define KEY-O      24)
(define KEY-P      25)
(define KEY-UP     103)
(define KEY-LEFT   105)
(define KEY-RIGHT  106)
(define KEY-DOWN   108)

; A KeyState is either 1 or 0; signifies whether a key is pressed or released.

; A FileDescriptor is an Integer representing a file descriptor.

; ------------------------------------------------------------------------------

; uinput FFI bindings

(define-ffi-definer define-uinput (ffi-lib "linux_uinput"))
; setup_uinput_device: Void -> FileDescriptor
; Initializes the uinput device and returns a file descriptor to /dev/uinput
(define-uinput setup_uinput_device (_fun -> _int))
; teardown_uinput_device: FileDescriptor -> Void
; Closes the given file descriptor
(define-uinput teardown_uinput_device (_fun _int -> _void))
; emit: FileDescriptor EventType EventCode KeyState
; Write an input event
(define-uinput emit (_fun _int _int _int _int -> _void))

; ------------------------------------------------------------------------------

; Implementation of device interface

; File descriptor
(define fd -1)

(define INPUT-KEY-MAPPING
  (hash
   'b1     KEY-Q
   'b2     KEY-W
   'b3     KEY-E
   'b4     KEY-R
   'b5     KEY-T
   'b6     KEY-Y
   'b7     KEY-U
   'b8     KEY-I
   'start  KEY-O
   'select KEY-P
   'up     KEY-UP
   'down   KEY-DOWN
   'left   KEY-LEFT
   'right  KEY-RIGHT))

; setup: Void -> Void
; Initializes the uinput device and sets up the file descriptor
(define (setup)
  (set! fd (setup_uinput_device))
  (when (< fd 0)
    (error 'setup "failed to initialize uinput device")))

; press: . Input -> Void
; Instantaneously hold and release the given Inputs
(define (press . inputs)
  (error-if-uninitialized 'press)
  (if (empty? inputs)
      (error 'press "no inputs given")
      (let ([keys (map input->key inputs)])
        (for-each (lambda (k) (emit fd EV-KEY k 1)) keys)
        (for-each (lambda (k) (emit fd EV-KEY k 0)) keys)
        (emit fd EV-SYN SYN-REPORT 0))))

; hold: . Input -> Void
; Start holding all of the given Inputs
(define (hold . inputs)
  (error-if-uninitialized 'hold)
  (if (empty? inputs)
      (error 'press "no inputs given")
      (let ([keys (map input->key inputs)])
        (for-each (lambda (k) (emit fd EV-KEY k 1)) keys)
        (emit fd EV-SYN SYN-REPORT 0))))

; release: . Input -> Void
; Release all of the given Inputs
(define (release . inputs)
  (error-if-uninitialized 'release)
  (if (empty? inputs)
      (error 'press "no inputs given")
      (let ([keys (map input->key inputs)])
        (for-each (lambda (k) (emit fd EV-KEY k 0)) keys)
        (emit fd EV-SYN SYN-REPORT 0))))

; teardown: Void -> Void
; Removes the uinput device and closes the file descriptor
(define (teardown)
  (error-if-uninitialized 'teardown)
  (teardown_uinput_device fd)
  (set! fd -1))

; error-if-uninitialized: Symbol -> Void
; Throw an error if the uinput device isn't running
(define (error-if-uninitialized sym)
  (when (< fd 0)
    (error sym "uinput device not initialized")))

; input->key: Input -> EventCode
; Return the kernel input event code that corresponds to the given Input
(define (input->key i)
  (hash-ref INPUT-KEY-MAPPING i))