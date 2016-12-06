#lang scheme

;;;;;;;;;;;;;;;;;;;;
;1 Comptes en banque
;;;;;;;;;;;;;;;;;;;;

(define (create-account init)
  (let ((i init)
        (d -300)) ;;a la création d’un compte le découvert autorise est de −300 euros
    (lambda (msg)

            ;;;;---
            ;;consulte le solde courant 
      (cond ((eq? msg 'solde) (lambda () (lambda () i)))

            ;;;;;;;;;;; Exemple du cour
            ((eq? msg 'incr) (lambda ()
                               (set! i (+ i 1)) 
                               i))
            ((eq? msg 'decr) (lambda () (lambda ()
                                          (set! i (- i 1))
                                          i)))
            ;;;;;;;;;; Fin Exemple du cour

            ;;---
            ;;effectue un versement dont on précise le montant, puis retourner le nouveau solde
            ((eq? msg 'versement) (lambda (somme) (lambda ()
                                               (set! i (+ i somme))
                                               i)))

            ;;---
            ;;modifi le découvert autorisé en la valeur donnée en argument et retourner cette valeur
            ((eq? msg 'decouvert) (lambda (somme) (lambda ()
                                                    (set! d (+ 0 somme))
                                                    d)))

            ;;---
            ;;effectue un retrait dont on précise le montant, à condition que cette opération ne mette
            ;;pas le solde en deçà du découvert autorisé ; si le retrait est possible, retourner le nouveau
            ;;solde, sinon retourner la valeur #f 
            ((eq? msg 'retrait) (lambda (somme)
                                 (cond ((< d (- i somme)) (lambda ()
                                                            (set! i (- i somme))
                                                            i
                                                            ))
                                       ((>= d (- i somme)) (lambda ()    
                                                             #f
                                                             ))
                                       )))

            ;;---
            ;;effectue un virement dont on précise le montant et le destinataire : les deux comptes doivent
            ;;être mis à jour ; si cette opération est possible, retourner le nouveau solde du débiteur, sinon
            ;;retourner la valeur #f ;
            ((eq? msg 'virement) (lambda (somme)
                                 (cond ((< d (- i somme)) (lambda (nom-destinataire) 
                                                            (set! i (- i somme))
                                                              (((nom-destinataire 'versement)somme))
                                                            i
                                                            ))

                                       
                                       ((>= d (- i somme)) (lambda (nom-destinataire)    
                                                             #f
                                                             ))
                                       )))


            
            )
      )
    )
  )


;;
;;Definition de plusieurs comptes
(define an-account-1 (create-account 100))
(define an-account-2 (create-account 200))
(define an-account-3 (create-account 300))
(define an-account-4 (create-account 400))



;;;;;;;;;;;;;;;;;;;;
;2 Central de banque
;;;;;;;;;;;;;;;;;;;;

;; Fonction du cours pour effectuer des operation sur des objet particulier
(define (look-up object a-list success-function failure-function)
  (cond ((assoc object a-list) => success-function)
        (else (failure-function))))


;;Permet de memoriser une liste d'association
(define (memoize f)
  (let ((f-a-list '()))
    ; Liste d’associations, qui sera progressivement enrichie par des effets
    ; de bord, et dont les éléments sont de la forme (i . j),
    ; avec (f i) =⇒ j.
    (lambda (x)
      (look-up x
               f-a-list
               cdr
               ; Obtention directe de l’image déjà mémorisée.
               ;; Fonction mémorisant l’image après l’avoir calculée :
               (lambda ()
                 (let ((result (f x)))
                   (set! f-a-list (cons (cons x result) f-a-list))
                   result))))))


(define liste-compte '((Mr1 . an-account-1 )
                      (Mr2 . an-account-2 )
                      (Mr3 . an-account-3 )
                      (Mr4 . an-account-4 )
                      )
  )


(define (central-banque list-init)
  (let ((li list-init))
    ;(memoize li)
    (lambda (msg)

      ;;;;---
      ;;retourne la liste des titulaires des comptes gérés par le central
      (cond ((eq? msg 'titulaire) (lambda () (map car li)))

            ;; Test entrer le un nouveau compte dans la liste
            ((eq? msg 'ajout-compte) (lambda (nom solde) (look-up (nom) li #f (lambda ()
                                                                                  (let ((li-ajout (cons nom (nom (create-account solde))))) 
                                                                                (set! li ((cons (li-ajout) (li)) )))
                                                                                ))
                                                                  ))
          
            )
      )
        
    )
  )

(define banque(central-banque liste-compte))