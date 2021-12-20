#lang racket

;
(provide
 ;
 createContext
 getEntityFromContextWithEId
 createEntityInContext
 getComponentFromEntityWithType
 createComponentForEntityInContext
 createSystemInContext
 updateSystemInContext
 swapUpdatedComponentDataInContext
 ;
 (struct-out context)
 (struct-out entity)
 (struct-out component)
 (struct-out system)
 )

; creation of a context/world
(struct context (ctxid
                 [ entities #:mutable ]
                 [ entities-by-type #:mutable ]
                 [ components #:mutable ]
                 [ components-by-type #:mutable ]
                 [ systems #:mutable ]
                 [ entityIdCounter #:mutable ]
                 [ componentIdCounter #:mutable])
  #:transparent)

; counter for context ids
(define contextIdCounter 0)

; create a new context
(define (createContext)
  (set! contextIdCounter (+ contextIdCounter 1))
  (let ((ctx (context contextIdCounter (make-hash) (make-hash) (make-hash) (make-hash) (make-hash) 0 0 )))
    ctx))

; get entity from context with its ID
(define (getEntityFromContextWithEId ctx eid)
  (hash-ref (context-entities ctx) eid))

; creation of an entity
(struct entity (eid etype ename [components #:mutable])
  #:transparent)

; create a new Entity
(define (createEntityInContext etype ename ctx)
  (let* ((eid (context-entityIdCounter ctx))
         (e (entity eid etype ename (make-hash))))
    (set-context-entityIdCounter! ctx (+ eid 1))
    (hash-set! (context-entities ctx) eid e)
    (when(not (hash-has-key? (context-entities-by-type ctx) etype))
        (hash-set! (context-entities-by-type ctx) etype (make-hash)))
    (hash-set! (hash-ref (context-entities-by-type ctx) etype) eid e)
    eid))

; get a particular component of an entity
(define (getComponentFromEntityWithType entity ctype)
  (filter (lambda (c) (eq? ctype (component-ctype c))) (hash-values (entity-components entity))))

; creation of a component
(struct component (cid ctype eid [ read-data #:mutable ] [ write-data #:mutable ] [ updated #:mutable ] )
  #:transparent)

; create a new component for an particular entity in a specific context
(define (createComponentForEntityInContext ctype cdata eid ctx)
  (let* ((cid (context-componentIdCounter ctx))
         (c (component cid ctype eid cdata cdata #f)))
    (set-context-componentIdCounter! ctx (+ cid 1))
    (hash-set! (context-components ctx) cid c)
    (hash-set! (entity-components (hash-ref (context-entities ctx) eid)) cid c)
    (when(not (hash-has-key? (context-components-by-type ctx) ctype))
        (hash-set! (context-components-by-type ctx) ctype (make-hash)))
    (hash-set! (hash-ref (context-components-by-type ctx) ctype) cid c)
    cid
    ))

; creation of a system in a specific context
(struct system (ctype [ components #:mutable ] func)
  #:transparent)

; create a new system in a particular context
(define (createSystemInContext ctype ctx func)
  (let* ((s (system ctype '() func)))
    (hash-set! (context-systems ctx) ctype s)
    ctype
    ))

; update a particular system for a particular context
(define (updateSystemInContext ctype ctx)
 (let* ((sys (hash-ref (context-systems ctx) ctype))
        (sys-update (system-func sys))
        (components (hash-values (context-components ctx)))
        (filtered-components (filter (lambda (c) (eq? ctype (component-ctype c))) components))
        (updated-components (map (lambda (comp)
                                   (sys-update comp ctx)
                                   (set-component-updated! comp #t))
                                 filtered-components)))
   (displayln "update sensors: ")
   filtered-components))

; swap data of updated component between read and write in a specific context
(define (swapUpdatedComponentDataInContext ctx)
  (let* ((pred? (lambda (c) (component-updated c)))
         (were-updated-components (filter pred? (hash-values (context-components ctx)))))
    (for-each (lambda (c)
                (let ((rdata (component-read-data c))
                      (wdata (component-write-data c)))
                  (set-component-read-data! c wdata)
                  (set-component-write-data! c rdata)
                  (set-component-updated! c #f)))
              were-updated-components)
    (displayln "swap updated: ")
    were-updated-components))
  
