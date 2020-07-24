Membri del gruppo:
Michele Fontana 829658
Antonio Suteu 847020

Questo README si riferisce al file ool.lisp che implementa un'estensione object oriented in Common Lisp
le funzioni principali come da consegna sono 4 :

define-class:
come suggerisce il nome permette di definire una nuova classe e di indicare i parent
(deve essere una lista) e i successivi slot che possono contenere o attributi o metodi.
I metodi devono iniziare con =>.

new:
permette di creare una nuova istanza di una classe, con un suo nome e i suoi attributi
automaticamente il programma rende invocabili i metodi creati in precedenza su quell'istanza.
 
<<:
dopo aver creato l'istanza posso eseguire la funzione << che avendo in input un'istanza e il nome 
di uno slot da cercare. ad esempio con age ritorna, se presente, il valore associato.
La ricerca del valore va nell'ordine prima nell'istanza stessa poi nella classe direttamente padre
e successivamente parte dal "parent più lontano a salire".

<<*:
analoga a << ma a ricerca multipla con più campi di ricerca.

altri metodi degni di nota e fondamentali sono process-method e rewrite-method-code che permettono di 
rendere invocabile i metodi con this.

funzionamento:
dopo aver definito delle classi e le istanze con new,il sistema in automatico rende invocabili i metodi 
creati e rende anche utilizzabile this (che sarà riferito all'istanza). 
-Le Classi: 
(define-class 'person '() 'name "Eve" 'age "undefined" 'altezza "undefined")
 
(define-class 'student '(person)
'name "Eva Lu Ator"
'university "Berkeley"
'altezza 1.80 
'talk '(=> (&optional (out *standard-output*))
(format out "My name is ~A~% My age is ~D~%"
(<< this 'name)
(<< this 'age))))

le Istanze:

(defparameter eve (new 'person))
(defparameter adam (new 'person 'name "Adam" 'altezza 1.75))
(defparameter s1 (new 'student 'name "Eduardo De Filippo" ))
(defparameter s2 (new 'student))

un'altra classe:

(define-class 'studente-bicocca '(student)
'talk '(=>()
(princ "Mi chiamo ")
(princ (<< this 'name))
(terpri)
(princ "e studio alla Bicocca.")
(terpri))
'university "UNIMIB")

l'ultima istanza:

(defparameter ernesto (new 'studente-bicocca'name "Ernesto" 'altezza 1.93))


L'utente ora può utilizzare le funzioni << e <<* per cercare eventuali dati richiesti 
o invocare i metodi creati nelle define class.
E come da consegna la richiesta (<< ernesto 'age) dovrà dare come risposta "undefined" dal suo parente
più lontano.
Può anche controllare l'esistenza di una classe con is-class (che prende in input una classe) o
l'esistenza di un'istanza con is-instance, il programma ritornerà true in caso positivo.