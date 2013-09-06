(multiplica-matriu
	'((1 2 3 1 2) (4 5 6 1 2) (7 8 9 1 1) (7 8 9 1 1)) 
	'((1 2 3 1 2) (4 5 6 1 2) (7 8 9 1 5) (7 8 9 1 5))
)

;N i M són les dues matrius a multiplicar.
;N ha de ser una llista amb més llistes a dins. És a dir:
n = ((1 2 3) (4 5 6) (7 8 9))
;M ha de ser una llista amb més llistes a dins. És a dir:
m = ((a b c) (d e f) (g h i))
(defun multiplica-matriu (n m)
	(cond
		((null (cdr m)) (construeix-columna n (car m)))
		(T (cons (construeix-columna n (car m))
				 (multiplica-matriu n (cdr m))
			)
		)
	)
)

;A cada iteració de "multiplica-matriu" hem de passar-li a "construeix-columna" N i M.
;N ha de ser una llista amb més llistes a dins. És a dir:
n = ((1 2 3) (4 5 6) (7 8 9))
;M ha de ser una sola llista (columna a tractar). P.e.:
m = (a b c)
(defun construeix-columna (n m)
	(cond
		((= 1 (length (car n))) (fila-x-columna n m))
		(T (cons (fila-x-columna n m)
				 (construeix-columna (mapcar 'cdr n) m)
			)
		)
	)
)



;A cada iteració de "construeix-columna" hem de passar-li a "fila_x_columna" N i M.
;N ha de ser una llista amb més llistes a dins però a cada iteració
;li treim tots els primers elements. És a dir:
n = ((1 2 3) (4 5 6) (7 8 9))
n = ((2 3) (5 6) (8 9))
n = ((3) (6) (9))
n = (() () ())
;M ha de ser una sola llista, és a dir, una sola columna. P.e.:
m = (a b c)
m = (b c)
m = (c)
m = ()
(defun fila-x-columna (n m)
	(cond
		((null m) 0)
		(T (+ 
			(* (car (car n)) (car m))
			(fila-x-columna (cdr n) (cdr m))
			)
		)
	)
)