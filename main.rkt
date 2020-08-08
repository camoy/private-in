#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     racket/require-transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shadow require

(define-syntax (-require stx)
  (syntax-parse stx
    [(_) #'(void)]
    [(_ ?spec)
     #'(begin
         (define-syntax var-ref (#%variable-reference))
         (require ?spec)
         (require-private ?spec))]
    [(_ ?spec ...) #'(begin (-require ?spec) ...)]))

(define-syntax (require-private stx)
  (define var-ref (syntax-local-value (datum->syntax stx 'var-ref)))
  (syntax-parse stx
    [(_ ?spec)
     #:with (?defn ...) (private-defns #'?spec var-ref)
     #'(begin ?defn ...)]))

(begin-for-syntax
  (define private-too? (make-parameter #f))

  (define (private-defns spec var-ref)
    (define-values (imports _)
      (parameterize ([private-too? var-ref])
        (expand-import spec)))
    (map import->defn (filter private-import? imports)))

  (define (private-import? i)
    (syntax-property (import-orig-stx i) 'private-mpi))

  (define (import->defn i)
    (match-define (import local-id src-sym _ _ _ orig-mode orig-stx) i)
    (define mpi (syntax-property orig-stx 'private-mpi))
    (define quoted-src-sym #`(quote #,src-sym))
    #`(define-syntax #,local-id
        (make-rename-transformer
         (syntax-binding-set->syntax
          (syntax-binding-set-extend
           (syntax-binding-set)
           #,quoted-src-sym
           #,orig-mode
           #,mpi)
          #,quoted-src-sym))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require transformers

(begin-for-syntax
  (define ((make-private-require-transformer public-too?) stx)
    (syntax-parse stx
      [(_ ?mp)
       ;; public imports
       (define mp (module-path-do-submod (syntax->datum #'?mp)))
       (define-values (-imports import-srcs)
         (expand-import #'?mp))
       (define imports (if public-too? -imports null))

       ;; private imports
       (define var-ref (private-too?))
       (define private-imports
         (cond
           [var-ref
            (define mpi-base (variable-reference->module-path-index var-ref))
            (define mpi (module-path-index-join mp mpi-base))
            (dynamic-require mpi #f)
            (define indirect-exports (module->indirect-exports mpi))
            (for*/list ([phase+syms (in-list indirect-exports)]
                        [sym (in-list (cdr phase+syms))])
              (define phase (car phase+syms))
              (define sym-stx (datum->syntax #f sym))
              (define local-id (syntax-local-introduce sym-stx))
              (define orig-stx (syntax-property sym-stx 'private-mpi mpi #t))
              (import local-id sym mp 0 0 phase orig-stx))]
           [else null]))

       (values (append imports private-imports) import-srcs)]))

  (define (module-path-do-submod spec)
    (match spec
      [`(quote ,id) `(submod "." ,id)]
      [_ spec]))
  )

(define-syntax private-in
  (make-require-transformer
   (make-private-require-transformer #t)))

(define-syntax only-private-in
  (make-require-transformer
   (make-private-require-transformer #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (module a racket/base
    (provide bar)
    (define foo 42)
    (define bar 43))

  (-require (prefix-in submod:both: (private-in 'a)))
  (-require (prefix-in submod:only: (only-private-in 'a)))
  (-require (prefix-in both: (private-in "test/mod.rkt")))
  (-require (prefix-in only: (only-private-in "test/mod.rkt")))

  (define-namespace-anchor a)

  (chk
   submod:both:foo 42
   both:foo 42

   submod:both:bar 43
   both:bar 43

   submod:only:foo 42
   only:foo 42

   #:! #:t
   (namespace-variable-value
    'only:bar
    #t
    (λ _ #f)
    (namespace-anchor->namespace a))
   ))
