#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require])
         private-in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/match
                     racket/list
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
         (begin-for-syntax
           (set-box! boxed-var-ref (#%variable-reference)))
         (require (public-in ?spec))
         (define-require-private ?spec))]
    [(_ ?spec ...) #'(begin (-require ?spec) ...)]))

;; Defines private identifiers.
(define-syntax (define-require-private stx)
  (syntax-parse stx
    [(_ ?spec)
     #:with (?defn ...) (private-defns #'?spec)
     #'(begin ?defn ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shadow require helpers

(begin-for-syntax
  ;; [Box [Or #f Variable-Reference]]
  ;; A box that contains a variable reference.
  (define boxed-var-ref (box #f))

  ;; Syntax → [Listof Syntax]
  ;; Returns a list of identifier definitions for private bindings.
  (define (private-defns spec)
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
    #`(define-syntax #,local-id (make-rename-transformer #'#,private-id)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require transformers

;; Require transformer for private and public bindings.
(define-syntax private-in
  (make-require-transformer
   (λ (stx)
     (syntax-parse stx
       [(_ ?mp)
        ;; public imports
        (define mp (module-path-do-submod (syntax->datum #'?mp)))
        (define-values (imports import-srcs)
          (expand-import #'?mp))

        ;; private imports
        (define var-ref (unbox boxed-var-ref))
        (unless var-ref
          (raise-syntax-error 'private-in
                              "use the require form provided by require-private"
                              stx))
        (define mpi-base (variable-reference->module-path-index var-ref))
        (define mpi (module-path-index-join mp mpi-base))
        (define mp-stx (syntax-property (datum->syntax #f mp) 'private-mpi mpi))
        (define private-syms (module->private-bindings mpi))
        (define private-imports
          (for*/list ([sym (in-list private-syms)])
            (define sym-stx (datum->syntax #f sym))
            (define local-id (syntax-local-introduce sym-stx))
            (import local-id sym mp-stx 0 0 0 sym-stx)))

        (values (append imports private-imports) import-srcs)]))))

;; Filter out private bindings (these will be defined by `define-require-private`
;; instead).
(define-syntax public-in
  (make-require-transformer
   (λ (stx)
     (syntax-parse stx
       [(_ ?spec)
        (define-values (imports import-srcs)
          (expand-import #'?spec))
        (values (filter (negate private-import-mpi) imports)
                import-srcs)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require transformer helpers

(begin-for-syntax
  ;; Module-Path-Index → [Listof Symbol]
  ;; Returns the private private run-time and transformer bindings for the given
  ;; module.
  (define (module->private-bindings mpi)
    (dynamic-require mpi #f)
    (define ns (module->namespace mpi))
    (define ids
      (parameterize ([current-namespace ns])
        (map namespace-symbol->identifier (namespace-mapped-symbols))))
    (define mpi-name
      (resolved-module-path-name (module-path-index-resolve mpi)))
    (filter-map
     (λ (x)
       (cond
         [(or (identifier-binding x)
              (identifier-binding (syntax-shift-phase-level x -1)))
          =>
          (λ (binding)
            (define source-name
              (resolved-module-path-name
               (module-path-index-resolve
                (first binding))))
            (define phase (fifth binding))
            (and (equal? mpi-name source-name)
                 (= phase 0)
                 (syntax-e x)))]
         [else #f]))
     ids))

  ;; Datum → Datum
  ;; Constructs a module path that handles submodules properly from a require
  ;; spec module path.
  (define (module-path-do-submod spec)
    (match spec
      [`(quote ,id) `(submod "." ,id)]
      [_ spec]))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (module a racket/base
    (provide bar)
    (define foo 42)
    (define bar 43)
    (define-syntax-rule (dbl x)
      (+ x x)))

  (module b racket/base
    (require (for-syntax racket/base))
    (begin-for-syntax
      (define foo 42)))

  (-require (prefix-in submod: (private-in 'a)))
  (-require (only-in (private-in 'a) [foo foo*]))
  (-require (prefix-in mod: (private-in "test/mod.rkt")))

  ;; Just to make sure this doesn't fail
  (-require (private-in 'b))

  (define-namespace-anchor a)

  (chk
   submod:foo 42
   (submod:dbl 7) 14
   foo* 42
   mod:foo 42

   submod:bar 43
   mod:bar 43
   ))
