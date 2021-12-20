#lang racket

;
(require "ecs.rkt")

; prepare test
(define ctx (createContext))
(define e1 (createEntityInContext 'aircraft 'e1 ctx))
(define e1c0 (createComponentForEntityInContext 'position (list 0 0) e1 ctx))
(define e1c1 (createComponentForEntityInContext 'sensor (list 1 2) e1 ctx))
(define e1c2 (createComponentForEntityInContext 'effector (list 3 4) e1 ctx))
(define e2 (createEntityInContext 'aircraft 'e2 ctx))
(define e2c0 (createComponentForEntityInContext 'position (list 100 0) e2 ctx))
(define e2c1 (createComponentForEntityInContext 'sensor (list 10 11) e2 ctx))
(define e3 (createEntityInContext 'boat 'e3 ctx))
(define e3c0 (createComponentForEntityInContext 'position (list 50 50) e3 ctx))

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