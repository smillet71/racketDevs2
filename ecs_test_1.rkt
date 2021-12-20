#lang racket

;
(require "ecs.rkt")

; prepare test
(define ctx (createContext))
(define ship (createEntityInContext 'ship 'the-ship ctx))
(define e1c0 (createComponentForEntityInContext 'power-generator (list 0) ship ctx))
(define e1c1 (createComponentForEntityInContext 'power-network (list 0) ship ctx))
(define e1c2 (createComponentForEntityInContext 'power-consummer (list 0) ship ctx))

;
(define sensorSys (createSystemInContext
                   'sensor ctx
                   (lambda (comp ctx)
                     (let* ((eid (component-eid comp))
                            (entity (getEntityFromContextWithEId ctx eid))
                            (position (first (getComponentFromEntityWithType entity 'position)))
                            (rdata (component-read-data comp))
                            (wdata (map (lambda (x) (+ x 1)) rdata)))
                       (displayln (list eid position))
                       (set-component-write-data! comp wdata)))
                   ))
; do test
(updateSystemInContext 'sensor ctx)
(swapUpdatedComponentDataInContext ctx)
(updateSystemInContext 'sensor ctx)
(swapUpdatedComponentDataInContext ctx)