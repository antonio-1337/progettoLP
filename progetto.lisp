;==============================================================================;
;                           PRIMITIVA:  DEFINE-CLASS                           ;
;==============================================================================;
; <Hash-Table> associata ad una variabile globale contenente le <Class-Specs>
; delle classi definite mediante funzione: (define-class <Class-Name> <Parent>)
;------------------------------------------------------------------------------;
(defparameter *classes-specs* (make-hash-table))
;------------------------------------------------------------------------------;
(defun add-class-spec (name class-spec)
    (setf (gethash name *classes-specs*) class-spec)
)
;------------------------------------------------------------------------------;
(defun get-class-spec (name)
    (gethash name *classes-specs*)
)
;==============================================================================;
;                             SUPER-CLASSE OBJECT                              ;
;==============================================================================;
; Definisco la classe <Object> per abilitare l'ereditarieta' di tipo NIL.
; La classe rappresenta il <Parent> massimo ereditabile durante definizione.
;------------------------------------------------------------------------------;
(add-class-spec 'object (list 'object NIL NIL))
;==============================================================================;
;     FUNZ. PRIMITIVA: (DEFINE-CLASS <CLASS-NAME> <PARENT> <SLOT-VALUE>*)      ;
;==============================================================================;
; Definisce una classe, controllando che l'argomento passato
; come <Parent> sia associato ad una classe gia' definita.
; Se la classe indicata come <Parent> non esiste, restituisce un errore.
; Es.: (define-class <class-name> <parent> <slot-value>*)
;------------------------------------------------------------------------------;
(defun define-class (class-name parent &rest slot-value)
    (cond
        ;----------------------------------------------------------------------;
        ; Se <Class-Name> e/o <Parent> non sono ATOMI o <Class-Name> nullo
        ;----------------------------------------------------------------------;
        (
            (or
                (not (symbolp class-name))
                (not (listp parent))
                (null class-name)
            )
            (error "ERRORE: <Class-Name> e/o <Class-Parent> invalidi/o!")
        )
        ;----------------------------------------------------------------------;
        ; Se <Parent> e <Class-Name> uguali
        ;----------------------------------------------------------------------;
        ((find class-name parent)
            (error "ERRORE: <Class-Parent> non puo' essere <Class-Name>!")
        )
        ;----------------------------------------------------------------------;
        ; Se <Parent> e' valorizzato, allora devono esistere delle
        ; *classes-specs* per <Parent>, altrimenti stampo errore!
        ;----------------------------------------------------------------------;
        (
            (and
                (not (null parent))
                (not (find-class-specs parent ));;helper che cerca tra l'elenco
            )
            (error "ERRORE: <Class-Parent> esistente, ma priva di <Slots>!")
        )
        ;----------------------------------------------------------------------;
        ; Se <Parent> e' NIL metto object come <Parent>.
        ; Altrimenti, sto definendo una classe che esiste gia', quindi devo
        ; rimuovere le *classes-specs* relative alla vecchia definizione.
        ;----------------------------------------------------------------------;
        (T
            (remhash class-name *classes-specs*)
            (progn
                ;--------------------------------------------------------------;
                ; Ottengo <Class-Spec> e genero <Hash>
                ;--------------------------------------------------------------;
                (let
					;se null super è object se no assegno parent 
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
                                (evaluator-lslots slot-value)
                                NIL ; PROCESS-BLOCKER ENABLED!
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
;==============================================================================;
;              FUNZ. PRIMITIVA: (NEW <CLASS-NAME> <SLOT-VALUE>*)               ;
;==============================================================================;
; Genero l'istanza della <Class-Name> passata come parametro elaborando
; eventuali membri e/o metodi sovrascritti a runtime nella forma: (K . V)*
;------------------------------------------------------------------------------;
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
                        (evaluator-lslots slot-value)
                        NIL ; PROCESS-BLOCKER DISABLED!
                    )
                )
            )
        )
        ;----------------------------------------------------------------------;
        (T (error "ERRORE: <Class-Name> da definire non trovata!"))
        ;----------------------------------------------------------------------;
    )
)
;==============================================================================;
;              FUNZ. PRIMITIVA: (<< <INSTANCE> <SLOT-NAME>)              ;
;==============================================================================;
; Recupera il valore <Slot-Value> associato al parametro passato <Slot-Name>
; mediante ricerca ricorsiva: se i parametri non sono corretti stampo errore!
;------------------------------------------------------------------------------;
(defun << (instance slot-name)
    (cond
        ;----------------------------------------------------------------------;
        ((not (symbolp slot-name))
            (error "ERRORE: Il campo di ricerca non e' un simbolo!")
        )
        ;----------------------------------------------------------------------;
        ((not (equalp 'oolinst (car instance)))
            (error "ERRORE: <Instance> da elaborare non valida!")
        )
        ;----------------------------------------------------------------------;
		((null (cddr instance))
			(<<helper (cadr (get-class-spec(cadr instance))) slot-name)
        )
        ;----------------------------------------------------------------------;
        ((equalp (caaddr instance) slot-name)
            (cond
                ;--------------------------------------------------------------;
                ; Se e' un ATOMO (e sotto-tipi)
                ;--------------------------------------------------------------;
                ((atom (cdr (caddr instance)))
                    (cdaddr instance)
                )
                ;--------------------------------------------------------------;
                ; Se e' un METODO ritorno la funzione LAMBDA associata
                ;--------------------------------------------------------------;
                (
                    (and
                        (equalp '=> (cadr (caddr instance)))
                        (>= (length (caddr instance)) 3)
                    )
                    (cadr (cdaddr instance))
                )
                ;--------------------------------------------------------------;
                ; Se e' alto (o una LISTA): esempio un membro con instance?
                ;--------------------------------------------------------------;
                (T
                    (cdaddr instance)
                )
                ;--------------------------------------------------------------;
            )
        )
        ;----------------------------------------------------------------------;
        (T
            (<<
                (cons
                    'oolinst
                    (cons (cadr instance) (cdddr instance))
                )
                slot-name
            )
        )
        ;----------------------------------------------------------------------;
    )
)

;==============================================================================;
;                  ELABORATORE EREDITARIETA' METODI E MEMBRI                   ;
;==============================================================================;
; Dati la lista <Slots> figlia, la lista <Slots> parent ed il valore
; della flag BLOCKER verifico la presenza di membri e/o metodi sovrascritti:
; concatenate le due liste filtrate ripulisco eventuali duplicati non rimossi.
;------------------------------------------------------------------------------;
(defun prepare-build-slots (listparent listson blocker)
    (remove-duplicates
        (build-slots listparent listson blocker)
        :key #'car
    )
)
;------------------------------------------------------------------------------;
; Date due liste le concatena verificando gli attributi delle liste della
; super classe e della classe figlia instanziando i valori di quest'ultima;
; altrimenti se mancano alla lista degli attributi della classe figlia vengono
; concatenati quelli della super classe
;------------------------------------------------------------------------------;
(defun build-slots (lparent lson blocker)
    (cond
        ;----------------------------------------------------------------------;
        ; Se <Parent> e <Figlio> entrambi NIL
        ;----------------------------------------------------------------------;
        (
            (and
                (null lparent)
                (null lson)
            )
            NIL
        )
        ;----------------------------------------------------------------------;
        ; Se <Parent> NIL e <Figlio> non NIL
        ;----------------------------------------------------------------------;
        ((null lparent)
            (cons
                (prepare-method (car lson) blocker)
                (build-slots NIL (cdr lson) blocker)
            )
        )
        ;----------------------------------------------------------------------;
        ; Se <Parent> non NIL e <Figlio> NIL
        ;----------------------------------------------------------------------;
        ((null lson)
            (cons
                (prepare-method (car lparent) blocker)
                (build-slots (cdr lparent) NIL blocker)
            )
        )
        ;----------------------------------------------------------------------;
        ; Se <Parent> e <Figlio> non NIL
        ;----------------------------------------------------------------------;
        (T
            (if (build-slots-checker lparent (caar lson))
                (cons
                    (prepare-method (car lson) blocker)
                    (build-slots
                        (build-slots-remover lparent (caar lson))
                        (cdr lson)
                        blocker
                    )
                )
                (cons
                    (prepare-method (car lson) blocker)
                    (build-slots lparent (cdr lson) blocker)
                )
            )
        )
        ;----------------------------------------------------------------------;
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
;==============================================================================;
;                         ELABORATORE DEI METHOD-SLOTS                         ;
;==============================================================================;
; Ottenuto lo slot dalla lista <Slots>, e il valore del bloccante,
; verifico se flaggato METHOD: se lo e' inizio l'elaborazione
; del metodo altrimenti ritorno lo slot in input.
;------------------------------------------------------------------------------;
(defun prepare-method (method-slots blocker)
    (cond
        ;----------------------------------------------------------------------;
        ; Se <Slot> è un metodo
        ;----------------------------------------------------------------------;
        (
            (and
                (not blocker)
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
        ;----------------------------------------------------------------------;
        ; Altrimenti ritorno lo <Slot> stesso
        ;----------------------------------------------------------------------;
        (T method-slots)
        ;----------------------------------------------------------------------;
    )
)
;------------------------------------------------------------------------------;
; Rendo il metodo invocabile nella forma: (<Method-Name> <Instance> <Args>)
; Restituisco il <Method-Slot> riadattato per poter accettare THIS.
;sksksksksksksksksk
;------------------------------------------------------------------------------;
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
    ;--------------------------------------------------------------------------;
    (eval (rewrite-method-code method-name method-spec))
)
;------------------------------------------------------------------------------;
; Avendo ottenuto <Method-Name> e <Method-Specs> rigenero lo <Slot>.
; Viene aggiunto il parametro THIS che, per convenzione, assumiamo
; essere sempre il primo elemento della lista argomenti.
;------------------------------------------------------------------------------;
(defun rewrite-method-code (method-name method-spec)
    (if
        (not (symbolp method-name))
        (error "ERRORE: Parametro <Method-Name> non e' un simbolo!")
    )
    ;--------------------------------------------------------------------------;
    (if
        (functionp (car method-spec))
        ;---| IF |-------------------------------------------------------------;
        (car method-spec)
        ;--| ELSE |------------------------------------------------------------;
        (append
            (list
                'lambda
                (if
                    ;----------------------------------------------------------;
                    (not (null (car method-spec)))
                    ;---| IF |-------------------------------------------------;
                    (rewrite-method-args
                        (if
                            (and
                                (listp (car method-spec))
                                (not (equalp 'this (caar method-spec)))
                            )
                            (append '(this) (car method-spec))
                            (car method-spec)
                        )
                    )
                    ;--| ELSE |------------------------------------------------;
					(list 'this)
                    ;----------------------------------------------------------;
					
                )
            )
            (cdr method-spec)
        )

    )
)
;------------------------------------------------------------------------------;
; Verifico la presenza di duplicati nella lista <Args>
; e, nel caso non vi fossero, ricostruisco la lista stessa.
;------------------------------------------------------------------------------;
(defun rewrite-method-args (largs)
    (cond
        ;----------------------------------------------------------------------;
        ((null largs) NIL)
        ;----------------------------------------------------------------------;
        ((listp largs)
            (if
                (not (member (car largs) (cdr largs)))
                    (cons
                        (car largs)
                        (rewrite-method-args (cdr largs))
                    )
                (error "ERRORE: Parametro duplicato!")
            )
        )
        ;----------------------------------------------------------------------;
        (T (error "ERRORE: Struttura <Slot-Args> non corretta!"))
        ;----------------------------------------------------------------------;
    )
)
;==============================================================================;
;                          GENERATORE LISTE DA SLOTS                           ;
;==============================================================================;
; Genero la struttura base di metodi e membri di una classe da una lista
; verificando se il numero dei vari elementi sia pari => (K . V)*
;------------------------------------------------------------------------------;
(defun evaluator-lslots (lslot-value)
    (cond
        ;----------------------------------------------------------------------;
        ((null lslot-value) NIL)
        ;----------------------------------------------------------------------;
        ((oddp (length lslot-value))
            (error "ERRORE: Lista nella forma (KEY . VALUE) non pari!")
        )
        ;----------------------------------------------------------------------;
        ((symbolp (car lslot-value))
            (cons
                (cons (car lslot-value) (cadr lslot-value))
                (evaluator-lslots (cddr lslot-value))
            )
        )
        ;----------------------------------------------------------------------;
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
			(T(if 	(null (procede (get-class-spec(car lista-parents)) slot-name))
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
;;ritorna il parent con più in "alto" che ha object come parent
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
	