#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require])
         private-in
         only-private-in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/match
                     syntax/parse
                     racket/require-transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shadow require

;; Replacement for `require` that handles private specs.
(define-syntax (-require stx)
  (syntax-parse stx
    [(_) #'(void)]
    [(_ ?spec)
     #'(begin
         (define-var-ref)
         (require (only-public-in ?spec))
         (define-require-private ?spec))]
    [(_ ?spec ...) #'(begin (-require ?spec) ...)]))

;; Initializes syntax local values needed for the private require transformers.
(define-syntax (define-var-ref stx)
  (syntax-local-introduce
   (if (syntax-local stx 'var-ref)
       #'(void)
       #'(define-syntax var-ref (#%variable-reference)))))

;; Defines private identifiers.
(define-syntax (define-require-private stx)
  (define var-ref (syntax-local stx 'var-ref))
  (syntax-parse stx
    [(_ ?spec)
     #:with (?defn ...) (private-defns #'?spec var-ref)
     #'(begin ?defn ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shadow require helpers

(begin-for-syntax
  ;; Syntax Symbol → [Or #f Any]
  ;; Returns the syntax binding for `sym` in the syntax's lexical context.
  (define (syntax-local stx sym)
    (syntax-local-value (datum->syntax stx sym) (λ () #f)))

  ;; Syntax Variable-Reference → [Listof Syntax]
  ;; Returns a list of identifier definitions for private bindings.
  (define (private-defns spec var-ref)
    (define-values (imports _)
      (expand-import spec))
    (map import->defn (filter private-import-mpi imports)))

  ;; Import → [Or #f Module-Path-Index]
  ;; Return the module path index of a private import.
  (define (private-import-mpi i)
    (define mp (import-src-mod-path i))
    (and (syntax? mp)
         (syntax-property mp 'private-mpi)))

  ;; Import → Syntax
  ;; Return the definition associated with an import.
  (define (import->defn i)
    (match-define (import local-id src-sym _ _ _ orig-mode orig-stx) i)
    (define sym (gensym))
    (define private-id
      (syntax-binding-set->syntax
       (syntax-binding-set-extend
        (syntax-binding-set)
        sym
        orig-mode
        (private-import-mpi i)
        #:source-symbol src-sym)
       sym))
    #`(define #,local-id #,private-id))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require transformers

(begin-for-syntax
  ;; Boolean → (Syntax → [Listof Import] [Listof Import-Src])
  ;; Constructs a require transformer for private bindings.
  (define ((make-private-require-transformer public-too?) stx)
    (syntax-parse stx
      [(_ ?mp)
       ;; public imports
       (define mp (module-path-do-submod (syntax->datum #'?mp)))
       (define-values (-imports import-srcs)
         (expand-import #'?mp))
       (define imports (if public-too? -imports null))

       ;; private imports
       (define var-ref (syntax-local stx 'var-ref))
       (define mpi-base (variable-reference->module-path-index var-ref))
       (define mpi (module-path-index-join mp mpi-base))
       (define mp-stx (syntax-property (datum->syntax #f mp) 'private-mpi mpi))
       (define indirect-exports (module->indirect-exports mpi))
       (define private-imports
         (for*/list ([phase+syms (in-list indirect-exports)]
                     [sym (in-list (cdr phase+syms))])
           (define phase (car phase+syms))
           (define sym-stx (datum->syntax #f sym))
           (define local-id (syntax-local-introduce sym-stx))
           (import local-id sym mp-stx 0 0 phase sym-stx)))

       (values (append imports private-imports) import-srcs)]))

  ;; Datum → Datum
  ;; Constructs a module path that handles submodules properly from a require
  ;; spec module path.
  (define (module-path-do-submod spec)
    (match spec
      [`(quote ,id) `(submod "." ,id)]
      [_ spec]))
  )

;; Require transformer for private and public bindings.
(define-syntax private-in
  (make-require-transformer
   (make-private-require-transformer #t)))

;; Require transformer for only private bindings.
(define-syntax only-private-in
  (make-require-transformer
   (make-private-require-transformer #f)))

;; Filter out private bindings.
(define-syntax only-public-in
  (make-require-transformer
   (λ (stx)
     (syntax-parse stx
       [(_ ?spec)
        (define-values (imports import-srcs)
          (expand-import #'?spec))
        (values (filter (negate private-import-mpi) imports)
                import-srcs)]))))

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
  (-require (only-in (only-private-in 'a) [foo foo*]))
  (-require (prefix-in both: (private-in "test/mod.rkt")))
  (-require (prefix-in only: (only-private-in "test/mod.rkt")))

  (define-namespace-anchor a)

  (chk
   submod:both:foo 42
   foo* 42
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
