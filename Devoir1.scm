#lang scheme



(define (create-account init)
  (let ((i init)
        (d -300))
    (lambda (msg)
      (cond ((eq? msg 'solde) (lambda () (lambda () i))) 
            ((eq? msg 'incr) (lambda ()
                               (set! i (+ i 1)) 
                               i))
            ((eq? msg 'decr) (lambda () (lambda ()
                                          (set! i (- i 1))
                                          i)))
            ((eq? msg 'versement) (lambda (somme) (lambda ()
                                               (set! i (+ i somme))
                                               i)))
            ((eq? msg 'decouvert) (lambda (somme) (lambda ()
                                                    (set! d (+ 0 somme))
                                                    d)))
            ((eq? msg 'retrait) (lambda (somme)
                                 (cond ((< d (- i somme)) (lambda ()
                                                            (set! i (- i somme))
                                                            i
                                                            ))
                                       ((>= d (- i somme)) (lambda ()    
                                                             #f
                                                             ))
                                       )))


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


(define an-account-1 (create-account 500))
(define an-account-2 (create-account 20))




