#! /usr/bin/env gsi -:dR

;;; Fichier : calc.scm

(define foldl
  (lambda (f base lst)
    (if (null? lst)
      base
      (foldl f (f base (car lst)) (cdr lst)))))


;;; Retourne le dernier element d'une liste.
(define last
  (lambda (lst)
    (if (null? (cdr lst))
      (car lst)
      (last (cdr lst)))))


;;; Retourne une liste qui contient tous les elements d'une liste sauf le dernier.
(define filter-last
  (lambda (lst)
    (if (null? (cdr lst))
      '()
      (cons (car lst) (append (filter-last (cdr lst)))))))


;;; Retourne une liste qui contient tous les elements sauf celui dont la cle est
;;; specifie en paremetre.
(define del-assoc
  (lambda (key lst)
    (if (or (null? lst) (assoc key lst))
      '()
      (cons (car lst) (append (del-assoc key (cdr lst)))))))


;;; Verifie si un charactere est un chiffre (entre 0 et 9).
(define char-numeric?
  (lambda (char)
    (and (char>=? char #\0) (char<=? char #\9))))


;;; Verifie si un charactere est une lettre (entre a et z).
(define char-letter?
  (lambda (char)
    (and (char>=? char #\a) (char<=? char #\z))))


;;; Verifie si un charactere est un operateur valide (+, - ou *).
(define char-operator?
  (lambda (char)
    (or (char=? char #\+) (char=? char #\-) (char=? char #\*))))


;;; Verifie si un charactere est l'operateur d'assignation (=).
(define char-assign?
  (lambda (char)
    (char=? char #\=)))


;;; Extrait les caracteres numeriques subsequents d'une liste, et ce, jusqu'a ce
;;; qu'un caractere qui n'est pas numerique soit rencontre.
(define next-token-num
  (lambda (lst)
    (if (or (null? lst) (not (char-numeric? (car lst))))
      (list lst)
      (cons (car lst) (append (next-token-num (cdr lst)))))))


;;; Cree un jeton compose d'un ou de plusieurs caracteres selon leur utilite.
(define next-token
  (lambda (lst)
    (cond
      ((null? lst) (list '() '()))
      ((not (list? lst)) (list (list lst) '()))
      ;;; Les caracteres numeriques forment un jeton de plusieurs caracteres.
      ((char-numeric? (car lst))
        (let ((res (next-token-num lst)))
          (list (filter-last res) (last res))))
      ;;; Les assignations forment un jeton de 2 caracteres.
      ((and (char-assign? (car lst)) (char-letter? (cadr lst)))
        (list (list (car lst) (cadr lst)) (cddr lst)))
      ;;; Les autres caracteres forment un jeton d'un seul caractere.
      (else (list (list (car lst)) (cdr lst))))))


;;; Separe une liste de caracteres en une liste de jetons pouvant etre plus
;;; facilement evaluee.
;;; Par exemple : '(1 2 3   4 5 6 +) => '((1 2 3) ( ) (4 5 6) (+))
(define tokenize
  (lambda (expr)
    (if (null? expr)
      '()
      (let ((res (next-token expr)))
        (append (list (car res)) (tokenize (cadr res)))))))


(define validate-helper
  (lambda (expr dict opcount vcount)
    (cond
      ;;; Retourne vrai si tous les tests ont reussi.
      ((null? expr) #t)
      ;;; Retourne un message d'erreur s'il y a trop d'operateurs pour le nombre
      ;; d'operandes.
      ((char-operator? (caar expr))
        (if (< vcount (+ opcount 2))
          (string->list "Expression invalide: pas assez d'operandes")
          (validate-helper (cdr expr) dict (+ opcount 1) vcount)))
      ;;; Retourne un message d'erreur s'il n'y a aucune valeur a donner a une
      ;;; assignation.
      ((char-assign? (caar expr))
        (if (< vcount 1)
          (string->list "Expression invalide: pas de valeur a assigner")
          (validate-helper (cdr expr)
                           (append dict (list (list (cadr (car expr)) 0)))
                           opcount
                           vcount)))
      ;;; Retourne un message d'erreur si une variable dans l'expression
      ;;; n'est pas definie.
      ((char-letter? (caar expr))
        (if (eqv? (assoc (caar expr) dict) #f)
          (string->list "Expression invalide: variable non definie")
          (validate-helper (cdr expr) dict opcount (+ vcount 1))))
      ;;; Les caracteres numeriques (0-9) sont toujours valides.
      ((char-numeric? (caar expr))
        (validate-helper (cdr expr) dict opcount (+ vcount 1)))
      ;;; Les espaces sont valides et sont ignorees.
      ((char=? #\space (caar expr))
        (validate-helper (cdr expr) dict opcount vcount))
      ;;; Retourne un message d'erreur si un caractere non supporte est rencontre.
      (else (string->list "Expression invalide: caractere iconnu")))))


;;; Verifie si une expression composee de plusieurs jetons est valide. Si c'est le
;;; cas, #t est retournee. Sinon, une liste de caracteres contenant le message
;;; d'erreur est retourne.
(define validate
  (lambda (expr dict)
    (validate-helper expr dict 0 0)))


;;; Retourne une fonction de reduction selon l'operation demandee.  Cet fonction
;;; peut etre utilisee directement pour effectuer des reductions avec la fonction
;;; foldl.
(define get-op
  (lambda (char)
    (cond
      ((char=? char #\+) (lambda (x y) (+ x y)))
      ((char=? char #\-) (lambda (x y) (- x y)))
      ((char=? char #\*) (lambda (x y) (* x y))))))


;;; Chaque caractere numerique de la liste est converti en entier et est additionne
;;; aux autres caracteres pour obtenir le nombre represente par la liste.
;;; Par exemple : '(1 2 3 4 5) => 12345
(define list->integer-helper
  (lambda (lst expo)
    (if (< expo 0)
      0
      (+ (* (- (char->integer (car lst)) 48) (expt 10 expo))
         (list->integer-helper (cdr lst) (- expo 1))))))


;;; Conversion d'une liste de caracteres numeriques (entre 0 et 9) vers un entier.
(define list->integer
  (lambda (lst)
    (list->integer-helper lst (- (length lst) 1))))


(define evaluate-helper
  (lambda (expr dict stack)
    (cond
      ;;; Retourne le resultat de l'evaluation lorsque tous les jetons ont ete
      ;;; traites.
      ((null? expr) (cons stack dict))
      ;;; Si le jeton courant est un entier on l'ajoute au stack et on continu
      ;;; l'evaluation des prochains jetons.
      ((char-numeric? (caar expr))
        (evaluate-helper (cdr expr)
                         dict
                         (append stack (list (list->integer (car expr))))))
      ;;; Si le jeton courant est une assignation de variable (=x), on l'ajoute
      ;;; au dict et on continu l'evaluation des prochains jetons.
      ((char-assign? (caar expr))
        (evaluate-helper (cdr expr)
                         (append (del-assoc (cadr (car expr)) dict)
                                 (list (list (cadr (car expr)) (last stack))))
                         stack))
      ;;; Si le jeton courant est une variable (x), on ajoute sa valeur au stack
      ;;; et on continu l'evaluation des prochains jetons.
      ((char-letter? (caar expr))
        (evaluate-helper (cdr expr)
                         dict
                         (append stack (cdr (assoc (caar expr) dict)))))
      ;;; Si le jeton courant est un operateur, on effectue l'operation sur tous
      ;;; les elements sur le stack et on continu l'evaluation des prochains jetons.
      ((char=? (caar expr) #\*)
        (evaluate-helper (cdr expr)
                         dict
                         (list (foldl (get-op (caar expr)) 1 stack))))
      ((char=? (caar expr) #\+)
        (evaluate-helper (cdr expr)
                         dict
                         (list (foldl (get-op (caar expr)) 0 stack))))
      ((char=? (caar expr) #\-)
        (evaluate-helper (cdr expr)
                         dict
                         (list (foldl (get-op (caar expr)) (car stack) (cdr stack)))))
      ;;; Ignore tous les autres caracteres et passe a l'evaluation des prochains
      ;;; jetons.
      (else (evaluate-helper (cdr expr) dict stack)))))


;;; Evalue une expression composee de plusieurs jetons et retourne le resultat.
;;; Par exemple : '((1 2 3) (4 5 6) (+)) => '(579)
(define evaluate
  (lambda (expr dict)
    (evaluate-helper expr dict '())))


;;; Traite l'expression entree par l'utilisateur, ligne par ligne, et affiche le
;;; resultat ou une erreur si l'expression est invalide.
(define traiter
  (lambda (expr dict)
    ;;; Separe l'expression en jetons.
    (let ((expr (tokenize expr)))
      ;;; Verifie si l'expression entree est valide.
      (let ((msg (validate expr dict)))
        (if (eqv? msg #t)
          ;;; Si l'expression est valide, on l'evalue et on retourne le resultat.
          (let ((res (evaluate expr dict)))
            (cons (append (string->list (number->string (caar res)))
                          '(#\newline))
                  (cdr res)))
          ;;; Sinon, on retourne le message d'erreur.
          (cons (append msg '(#\newline)) dict))))))

;;;----------------------------------------------------------------------------

;;; Ne pas modifier cette section.

(define repl
  (lambda (dict)
    (print "# ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (let ((r (traiter-ligne ligne dict)))
            (for-each write-char (car r))
            (repl (cdr r)))))))


(define traiter-ligne
  (lambda (ligne dict)
    (traiter (string->list ligne) dict)))


(define main
  (lambda ()
    (repl '()))) ;; dictionnaire initial est vide

;;;----------------------------------------------------------------------------
