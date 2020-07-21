;Fontana Michele 829658


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
                        (incapsulate-slots slot-value)
                        )
                    )
                )
				;ritorno class name 
                class-name
            )
        )
    )
)

;genero l'instance della class name passata aggiungo oolinst come da consegna per riconoscerla
(defun new (class-name &rest slot-value)
    (cond
        ((get-class-spec class-name)
            (cons
                'oolinst
                (cons
                    class-name
                    (prepare-build-slots
                        (caddr (get-class-spec class-name))
                        (incapsulate-slots slot-value)
						(cadr (get-class-spec class-name))
                    )
                )
            )
        )
        (T (error "ERRORE: <Class-Name> da definire non trovata!"))
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
		((null (cddr instance))
			(error "ERRORE: <Slot-name> non trovato!")
        )
		;ricerca normale nell'istance 
        ((equalp (caaddr instance) slot-name)
            (cond
                ((atom (cdr (caddr instance)))
                    (cdaddr instance)
                )
				; se è =>, ritorno la funzione
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

;;esegue ricerca << su più slot 
(defun <<* ( istance slt1 &rest slts)
	(write (<< istance slt1))
	(terpri)	
	(<<all istance slts)
)


;; esegue << su una lista 
(defun <<all (istance slts)
	(if 
		(null slts)
		;if
		T
		;else
		(progn 
			(write (<< istance (car slts)))
			(terpri)
			(<<all istance (cdr slts))
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


;elabora l'ereditarietà in profondità dei metodi e degli slot con il parent quando creo un'istanza 
(defun prepare-build-slots (listparent listson parents)
	(remove-duplicates
		(reduce #'cons 
			(build-slots listparent listson)
			:initial-value 
					(reduce #'cons
					(exec-parents parents listson)
					:initial-value (exec-all parents listson)
					:from-end t )
			:from-end t)
        :key #'car :from-end t
    
	)
	
)
;cerca gli slots da mettere prima nei parents più lontani
(defun exec-parents (listpar son)
	(if
		(or (null listpar) (not (listp listpar)))
		NIL
		(reduce #'cons 
		(build-slots (caddr(get-class-spec(get-parent-to-obj(list(car listpar)))))son)
		:initial-value (exec-all (cdr listpar) son)
		:from-end t)
	)
	)

;cerca gli slots nei parents più vicini ovviamente con priorità a quelli più lontani
(defun exec-all (listpar son)
	(if
		(or (null listpar) (not (listp listpar)))
		NIL
		(reduce #'cons 
		(build-slots (caddr(get-class-spec(car listpar)))son)
		:initial-value (exec-all (cdr listpar) son)
		:from-end t)
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
                        (arg-remover lparent (caar lson))
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
; ricerca slot in esame nelle due liste T se trovato
(defun build-slots-checker (lcurrent findarg)
    (cond
        ((not (symbolp findarg))
            (error "ERRORE: Lo <Slot-Name> non e' un simbolo!")
        )
		
        ((null lcurrent)
            (return-from build-slots-checker NIL)
        )

        ( 
            (if (equalp (caar lcurrent) findarg)
                (return-from build-slots-checker T)
                (build-slots-checker
                    (cdr lcurrent)
                    findarg
                )
            )
        )
    )
)

; elimino temporaneamente dalla super-classel'attributo della figlia in esame per avere uno e un solo slot per nome.
(defun arg-remover (lcurrent delarg)
    (cond
        ((null lcurrent) NIL)

        (T
            (if (equalp (caar lcurrent) delarg)
                (arg-remover
                    (cdr lcurrent)
                    delarg
                )
                (cons
                    (car lcurrent)
                    (arg-remover
                        (cdr lcurrent)
                        delarg
                    )
                )
            )
        )
    )
)
;controlla se è presente => (quindi se è un metodo ) nel caso positivo fa process method
(defun check-is-method (method-slots)
    (cond
        (
            (and
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


;;true se esistono le class specs
(defun find-class-specs (lista-parents)
	(cond 
		( (null lista-parents)  T)
		((null (get-class-spec (car lista-parents)))   NIL	)
		(T (find-class-specs (cdr lista-parents)))
	)
)

;;ritorna il parent più in "alto" nella gerarchia cioè che ha object come parent
(defun get-parent-to-obj (lista-parents)
	(let 
		((obj 
			(if 
				(listp (cadr(get-class-spec(car lista-parents))))
				;if
				(caadr(get-class-spec(car lista-parents)))
				;else
				(cadr(get-class-spec(car lista-parents))))
		))
		(if 
			(null lista-parents)
			;if
			NIL
			;else
			(if 
				(equalp 'object obj) 
				;if
				(return-from get-parent-to-obj (car(get-class-spec(car lista-parents))))
				;else
				(get-parent-to-obj (cadr(get-class-spec(car lista-parents))))
			)
		)
	)
)

