;; -*- mode: scheme -*-

(library (Effect.Exception foreign)
  (export showErrorImpl
          error
          errorWithCause
          message
          name
          stackImpl
          throwException
          catchException)
  (import (only (rnrs base) define lambda cond else quote)
          (only (rnrs conditions) define-condition-type &condition condition? message-condition? condition-message)
          (only (rnrs io ports) call-with-string-output-port)
          (only (rnrs exceptions) with-exception-handler raise-continuable)
          (only (chezscheme) format call/cc display-condition))

  ; ----- Internal condition specific to Effect.Exception
  ; Effect.Exception can catch other exceptions as well but
  ; makes a distinction between exceptions thrown via its interface 
  ; and all other exceptions so as to implement `errorWithCause` (parent exceptions).
  (define-condition-type
    &purescm-effect-exception
    &condition
    make-purescm-effect-exception
    purescm-effect-exception? 
    (message purescm-effect-exception-message)
    (cause purescm-effect-exception-cause))

  ; ----- Exported FFI implementation

  (define showErrorImpl
    (lambda (err)
      (cond
        ((condition? err)
          (call-with-string-output-port
            (lambda (p) (display-condition err p))))
        (else
          (format "Exception: ~s" err)))))

  (define error
    (lambda (msg)
      (make-purescm-effect-exception msg '())))

  (define errorWithCause
    (lambda (msg)
      (lambda (cause)
        (make-purescm-effect-exception msg cause))))

  (define message
    (lambda (e)
      (cond
        ((purescm-effect-exception? e)
          (purescm-effect-exception-message e))
        ((message-condition? e)
          (condition-message e))
        (else
          (format "Exception: ~s" e)))))

  (define name
    (lambda (e)
      "Exception"))

  (define stackImpl
    (lambda (just)
      (lambda (nothing)
        (lambda (e)
          nothing))))

  (define throwException
    (lambda (e)
      (lambda ()
        (raise-continuable e))))

  (define catchException
    (lambda (c)
      (lambda (t)
        (lambda ()
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (e) (k ((c e))))
                (lambda () (t)))))))))
)
