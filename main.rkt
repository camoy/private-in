#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require])
         private-in
         only-private-in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
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
         (define-syntax var-ref (#%variable-reference))
         (require ?spec)
         (require-private ?spec))]
    [(_ ?spec ...) #'(begin (-require ?spec) ...)]))

;; Defines private identifiers.
(define-syntax (require-private stx)
  (define var-ref (syntax-local-value (datum->syntax stx 'var-ref)))
  (syntax-parse stx
    [(_ ?spec)
     #:with (?defn ...) (private-defns #'?spec var-ref)
     #'(begin ?defn ...)]))

(begin-for-syntax
  ;; [Parameter [Or #f Variable-Reference]]
  ;; Is either false (meaning the require transformer should not import private
  ;; identifiers) or a variable reference from the use site allowing us to
  ;; inspect private identifiers.
  (define private-too? (make-parameter #f))

  ;; Syntax Variable-Reference → [Listof Syntax]
  ;; Returns a list of identifier definitions for private bindings.
  (define (private-defns spec var-ref)
    (define-values (imports _)
      (parameterize ([private-too? var-ref])
        (expand-import spec)))
    (map import->defn (filter private-import? imports)))

  ;; Import → Boolean
  ;; Return if the import is private.
  (define (private-import? i)
    (syntax-property (import-orig-stx i) 'private-mpi))

  ;; Import → Syntax
  ;; Return the definition associated with an import.
  (define (import->defn i)
    (match-define (import local-id src-sym _ mode _ orig-mode orig-stx) i)
    (define define-for-?
      (case mode
        [(0) #'define]
        [(1) #'define-for-syntax]
        [else
         (raise-syntax-error 'require-private
                             "can only require for phase 0 or 1")]))
    (define private-id
      (syntax-binding-set->syntax
       (syntax-binding-set-extend
        (syntax-binding-set)
        src-sym
        orig-mode
        (syntax-property orig-stx 'private-mpi))
       src-sym))
    #`(#,define-for-? #,local-id #,private-id))
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
       (define var-ref (private-too?))
       (define private-imports
         (cond
           [var-ref
            (define mpi-base (variable-reference->module-path-index var-ref))
            (define mpi (module-path-index-join mp mpi-base))
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
