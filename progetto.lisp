;parametro associato alle specs della classe
(defparameter *classes-specs* (make-hash-table))
;permette di settare la hash
(defun add-class-spec (name class-spec)
    (setf (gethash name *classes-specs*) class-spec)
)
;getter della hash
(defun get-class-spec (name)
    (gethash name *classes-specs*)
)
;permette la parentela con la classe object
(add-class-spec 'object (list 'object NIL NIL))
;funzione primitiva define class prende in input 
(defun define-class (class-name parent &rest slot-value)
    (cond
        (
            (or
                (not (symbolp class-name))
                (not (listp parent))
                (null class-name)
            )
            (error "ERRORE: <Class-Name> e/o <Class-Parent> invalidi/o!")
        )
		
        ((find class-name parent)
            (error "ERRORE: <Class-Parent> non puo' essere <Class-Name>!")
        )
		;se parent not null devono esistere delle specs e lo cerco nella lista con find-class-specs
        (
            (and
                (not (null parent))
                (not (find-class-specs parent ));;helper che cerca tra l'elenco
            )
            (error "ERRORE: <Class-Parent> esistente, ma priva di <Slots>!")
        )
        (T
            (remhash class-name *classes-specs*)
            (progn
                (let
					;se parent è null, super è object se no assegno la lista parent 
                    ((super
                        (if
                            (null parent)
                            'object
							;else 
                            parent
                        )
                    ))
                    (add-class-spec class-name ;nome della hash-table
						;add specs ordine corretto nome classe super 
                        (list
                            class-name super
                            (prepare-build-slots
                                (caddr (get-class-spec super)) ;third in poi
                                (incapsulate-slots slot-value)
                            )
                        )
                    )
                )
                ;--------------------------------------------------------------;
                ; Ritorno <Class-Name> come da specifica
                ;--------------------------------------------------------------;
                class-name
                ;--------------------------------------------------------------;
            )
        )
    )
)
;genero l'instance della class name passata aggiungo oolinst come da consegna per riconoscerla
(defun new (class-name &rest slot-value)
    (cond
        ;----------------------------------------------------------------------;
        ((get-class-spec class-name)
            (cons
                'oolinst
                (cons
                    class-name
                    (prepare-build-slots
                        (caddr (get-class-spec class-name))
                        (incapsulate-slots slot-value)
                    )
                )
            )
        )
        ;----------------------------------------------------------------------;
        (T (error "ERRORE: <Class-Name> da definire non trovata!"))
        ;----------------------------------------------------------------------;
    )
)
;funzione che ritorna il valore dello slot name richiesto dall'instance richiesta 
(defun << (instance slot-name)
    (cond
        ((not (symbolp slot-name))
            (error "ERRORE: Il campo di ricerca non e' un simbolo!")
        )
        ((not (equalp 'oolinst (car instance)))
            (error "ERRORE: <Instance> da elaborare non valida!")
        )
		; se non ho più nulla in cui cercare cerco nei parents partendo dal più lontano
		((null (cddr instance))
			(<<helper (cadr (get-class-spec(cadr instance))) slot-name)
        )
		;ricerca normale nell'istance 
        ((equalp (caaddr instance) slot-name)
            (cond
                ((atom (cdr (caddr instance)))
                    (cdaddr instance)
                )
				; se => ritorno la funzione
                (
                    (and
                        (equalp '=> (cadr (caddr instance)))
                        (>= (length (caddr instance)) 3)
                    )
                    (cadr (cdaddr instance))
                )

                (T
                    (cdaddr instance)
                )
            )
        )
		;chiamata ricorsiva degli slot rimanenti (dell'instance)
        (T
            (<<
                (cons
                    'oolinst
                    (cons (cadr instance) (cdddr instance))
                )
                slot-name
            )
        )
    )
)

;==============================================================================;
;                  ELABORATORE EREDITARIETA' METODI E MEMBRI                   ;
;==============================================================================;
; Dati la lista <Slots> figlia, la lista <Slots> parent ed il valore
; della flag BLOCKER verifico la presenza di membri e/o metodi sovrascritti:
;------------------------------------------------------------------------------;
(defun prepare-build-slots (listparent listson )
    (remove-duplicates
        (build-slots listparent listson)
        :key #'car
    )
)

; Date due liste le concatena verificando gli attributi delle liste della
; super classe e della classe figlia instanziando i valori di quest'ultima;
; altrimenti se mancano alla lista degli attributi della classe figlia vengono
; concatenati quelli della super classe
(defun build-slots (lparent lson )
    (cond
        (
            (and
                (null lparent)
                (null lson)
            )
            NIL
        )
		
        ((null lparent)
            (cons
                (check-is-method (car lson) )
                (build-slots NIL (cdr lson) )
            )
        )

        ((null lson)
            (cons
                (check-is-method (car lparent) )
                (build-slots (cdr lparent) NIL )
            )
        )

        (T
            (if (build-slots-checker lparent (caar lson))
                (cons
                    (check-is-method (car lson) )
                    (build-slots
                        (build-slots-remover lparent (caar lson))
                        (cdr lson)
                        
                    )
                )
                (cons
                    (check-is-method (car lson) )
                    (build-slots lparent (cdr lson) )
                )
            )
        )
    )
)
;------------------------------------------------------------------------------;
; Date due liste verifico gli attributi della super-classe e della figlia
; in cerca dello slot in esame. Se lo trovo forzo il ritorno di T.
;------------------------------------------------------------------------------;
(defun build-slots-checker (lcurrent findarg)
    (cond
        ;----------------------------------------------------------------------;
        ; Se argomento da cercare non e' un simbolo
        ;----------------------------------------------------------------------;
        ((not (symbolp findarg))
            (error "ERRORE: Lo <Slot-Name> non e' un simbolo!")
        )
        ;----------------------------------------------------------------------;
        ; Se lista immessa NIL
        ;----------------------------------------------------------------------;
        ((null lcurrent)
            (return-from build-slots-checker NIL)
        )
        ;----------------------------------------------------------------------;
        ; Ricorsione utile alla navigazione della lista in cerca di ARG
        ;----------------------------------------------------------------------;
        ( 
            (if (equalp (caar lcurrent) findarg)
                (return-from build-slots-checker T)
                (build-slots-checker
                    (cdr lcurrent)
                    findarg
                )
            )
        )
        ;----------------------------------------------------------------------;
    )
)
;------------------------------------------------------------------------------;
; Date due liste pre-verificate elimino temporaneamente dalla super-classe
; l'attributo della figlia in esame per avere uno e un solo slot per nome.
;------------------------------------------------------------------------------;
(defun build-slots-remover (lcurrent delarg)
    (cond
        ;----------------------------------------------------------------------;
        ; Se lista immessa NIL
        ;----------------------------------------------------------------------;
        ((null lcurrent) NIL)
        ;----------------------------------------------------------------------;
        ; Ricorsione per manipolazione della lista immessa: REMOVE( ARG )
        ;----------------------------------------------------------------------;
        (T
            (if (equalp (caar lcurrent) delarg)
                (build-slots-remover
                    (cdr lcurrent)
                    delarg
                )
                (cons
                    (car lcurrent)
                    (build-slots-remover
                        (cdr lcurrent)
                        delarg
                    )
                )
            )
        )
        ;----------------------------------------------------------------------;
    )
)
;controlla se è presente => (quindi se è un metodo ) nel caso positivo fa process method
(defun check-is-method (method-slots );blocker
    (cond
        (
            (and
                ;(not blocker)
                (listp (cdr method-slots))
                (equalp '=> (cadr method-slots))
            )
            (append
                (list
                    (car method-slots)
                    '=>
                )
                (list
                    (process-method
                        (car method-slots)
                        (cddr method-slots)
                    )
                )
            )
        )
        (T method-slots)
    )
)
; Rendo il metodo invocabile nella forma: (<Method-Name> <Instance> <Args>)
(defun process-method (method-name method-spec)
    (setf (fdefinition method-name) ;; Associate The Lambda To The Method's Name
        (lambda
            (this &rest args) ;; Adds The THIS Argument
            (apply
                (<< this method-name)
                (append (list this) args) 
            )
        )
    )
	;(eval lambda (this)...ecc)
    (eval (rewrite-method-code method-name method-spec))
)
;avendo method name e method specs aggiungo il parametro this messo in testa alla lista 
(defun rewrite-method-code (method-name method-spec)
    (if
        (not (symbolp method-name))
        (error "ERRORE: Parametro <Method-Name> non e' un simbolo!")
    )
    (if
        (functionp (car method-spec))
        (car method-spec)
        (append
            (list
                'lambda
                (if
                    (not (null (car method-spec)))
                    (if
                          (and
                                (listp (car method-spec))
                                (not (equalp 'this (caar method-spec)))
                            )
                            (append '(this) (car method-spec))
                            (car method-spec)
                        )
                    
					(list 'this)
					
                )
            )
            (cdr method-spec)
        )

    )
)
;incapsula nella forma (KEY . VALUE) utile per la ricerca 
(defun incapsulate-slots (lslot-value)
    (cond

        ((null lslot-value) NIL)

        ((oddp (length lslot-value))
            (error "ERRORE: Lista nella forma (KEY . VALUE) non pari!")
        )

        ((symbolp (car lslot-value))
            (cons
                (cons (car lslot-value) (cadr lslot-value))
                (incapsulate-slots (cddr lslot-value))
            )
        )

        (T
            (error "ERRORE: Formato <Slot-Name> invalido di metodi o membri!")
        )
    )
)
;;ritorna true se il valore passato è una classe
(defun is-class (class-name)
	(if
		(null (get-class-spec class-name)) 
		
		NIL
		
		T
)
)
;;ritorna true se il valore passato è un'istanza 
(defun is-instance (instance)
	(if
		(or (not (listp instance)) (null ( cddr instance)))
		
		NIL
		
		(if 	
			(equalp 'oolinst (car instance))   
			T
			NIL
		
		)
	
)
)
;;true se esistono le class specs
(defun find-class-specs (lista-parents)
	(cond 
		((null lista-parents) T)
		((null (get-class-spec (car lista-parents))) NIL	)
		(T (find-class-specs (cdr lista-parents)))
		))

;;gestisce la ricerca invocando procede 
(defun <<helper (lista-parents slot-name)
	(if (listp lista-parents)
	(if		(null (car lista-parents)) 
		(error "ERRORE: <Slot-Name> da ricercare sconosciuto!")
		(cond  
			((not (null (procede (get-class-spec(get-parent-to-obj (list (car lista-parents)))) slot-name))) 
				(procede (get-class-spec(get-parent-to-obj (list (car lista-parents)))) slot-name))
			(T(if 	
				(null (procede (get-class-spec(car lista-parents)) slot-name))
						(if (null (cdr lista-parents))
						(<<helper (cadr (get-class-spec (car lista-parents))) slot-name)
						(<<helper (cdr lista-parents) slot-name)
						)
						(procede (get-class-spec(car lista-parents))slot-name)
				)
			
			
			)
		)
	)
	(error "ERRORE: no method or slot named <Slot-Name> found.")
)
)
;;elabora la ricerca 
(defun procede (parent slot-name)
(cond
	((null (caddr parent))
            NIL
       )
	 ((equalp (car(caaddr parent)) slot-name)
           (cdr(caaddr parent ))
        )
		 (T
            (procede
                (reduce #'cons
                     (subseq parent 0 2)
                     :initial-value (list(cdaddr parent))
                     :from-end t)
                slot-name
            )
        )
	)
	)
;;ritorna il parent più in "alto" nella gerarchia cioè che ha object come parent
(defun get-parent-to-obj (lista-parents)
	(let ((obj 
		(if (listp (cadr(get-class-spec(car lista-parents))))
			(caadr(get-class-spec(car lista-parents)))
			(cadr(get-class-spec(car lista-parents))))
	))
	(if (null lista-parents)
		NIL
	(if (equalp 'object obj) 	
		(return-from get-parent-to-obj (car(get-class-spec(car lista-parents))))
		(get-parent-to-obj (cadr(get-class-spec(car lista-parents))))
	)
	)
	))
;;esegue ricerca << su più slot 
(defun <<* ( istance slt1 &rest slts)
	(write (<< istance slt1))
	(terpri)	
	(<<all istance slts)
)
;; esegue << su una lista 
(defun <<all (istance slts)
	(if (null slts)
		T
		(progn 
		(write (<< istance (car slts)))
		(terpri)
		(<<all istance (cdr slts))
		)
		;(values(<< istance (car slts)) (<<all istance (cdr slts)))
)
)
	
