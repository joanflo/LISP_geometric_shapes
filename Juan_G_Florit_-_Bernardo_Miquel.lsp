;==============================================================
;========================== DIBUIX 3D =========================
;Autors:
;   - Juan Gabriel Florit Gomila
;   - Bernardo Miquel Riera
;==============================================================

;==============================================================
;==================== DEFINICIÓ DE PATRONS ====================
;==============================================================

;Patró cub (x z y):
	(putprop 'cub 'cub 'nom)
	(putprop 'cub '((-0.5 0 -0.5) (0.5 0 -0.5) (0.5 0 0.5)
	(-0.5 0 0.5) (-0.5 1 -0.5) (0.5 1 -0.5) (0.5 1 0.5)
	(-0.5 1 0.5)) 'punts)
	(putprop 'cub '((1 2) (2 3) (3 4) (4 1) (1 5) (2 6) (3 7)
	(4 8) (5 6) (6 7) (7 8) (8 5)) 'arestes)
	(putprop 'cub '((1 2 3 4) (9 10 11 12) (1 5 9 6) (2 7 10 6)
	(3 8 11 7) (4 8 12 5)) 'cares)
	(putprop 'cub '(255 0 0) 'color)
	
;Patró prisma (x z y):
	(putprop 'prisma 'prisma 'nom)
	(putprop 'prisma '((0 0 -0.5) (0.5 0 0.5) (-0.5 0 0.5)
	(0 1 -0.5) (0.5 1 0.5) (-0.5 1 0.5)) 'punts)
	(putprop 'prisma '((1 2) (2 3) (3 1) (1 4) (2 5) (3 6)
	(4 5) (5 6) (6 4)) 'arestes)
	(putprop 'prisma '((1 2 3) (7 8 9) (1 5 7 4) (2 6 8 5)
	(3 6 9 4)) 'cares)
	(putprop 'prisma '(255 0 0) 'color)
	
;Patró octaedre (x z y):
	(putprop 'octaedre 'octaedre 'nom)
	(putprop 'octaedre '((-0.5 0.5 -0.5) (0.5 0.5 -0.5)
	(0.5 0.5 0.5) (-0.5 0.5 0.5) (0 1 0) (0 0 0)) 'punts)
	(putprop 'octaedre '((1 2) (2 3) (3 4) (4 1) (1 5) (2 5)
	(3 5) (4 5) (1 6) (2 6) (3 6) (4 6)) 'arestes)
	(putprop 'octaedre '((1 5 6) (2 7 6) (3 8 7) (4 8 5)
	(1 10 9) (2 10 11) (3 11 12) (4 12 9)) 'cares)
	(putprop 'octaedre '(255 0 0) 'color)

	
	
;==============================================================
;================= DEFINICIÓ DE LES FUNCIONS ==================
;==============================================================

;Hi haurà una llista de figures que anirà guardant totes les
;figures que es vagin creant (inicialment buida):
(setq figures nil)


;(crea-figura nom patró color): És una funció que permet la
;creació d’una figura 3D a partir del patró triat.
(defun crea-figura (nom patro color)
	(putprop nom (get patro 'punts) 'punts)
	(putprop nom (get patro 'arestes) 'arestes)
	(putprop nom (get patro 'cares) 'cares)
	(putprop nom color 'color)
	(putprop nom m-identitat 'matriu)
	(setq figures (cons nom figures))
)


;Al crear la figura, per defecte, tindrà una propietat que serà
;la seva matriu identitat. Això es fa per a no haver de definir la
;matriu idetitat cada cop que es fa una nova figura. La matriu
;anirà canviant segons les transformacions que se li apliquin:
(setq m-identitat '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))    


;(borra-figura f): borra la figura f de la llista “figures”
(defun borra-figura (f)
	(setq figures (delete f figures))
)


;(borra-figures): borra tot el contingut de la llista “figures”
(defun borra-figures ()
	(setq figures nil)
)


;(pinta-figura f): dibuixa la figura f a partir de les coordenades
;(x,y) de cada punt, la z no s’ha d’utilitzar pel dibuixat,
;únicament pels càlculs posteriors
(defun pinta-figura (f)
	(pinta-arestes f (get f 'arestes))
)
;Pinta totes les arestes de la figura:
(defun pinta-arestes (f arestes)
	(cond ((null arestes) (print 'figura_pintada))
		  (T (pinta-aresta f (car arestes)) 
			 (pinta-arestes f (cdr arestes))
		  )
	)
)
;Pinta una aresta donada (es bota al punt inicial de l'aresta
;i es bota fins el punt final):
(defun pinta-aresta (f aresta)
	(color (car(get f 'color))
		   (cadr(get f 'color))
		   (caddr(get f 'color))
	)
    (bota (round (extreu-x-1p f aresta)) 
	(round(extreu-y-1p f aresta)))
	(pinta-fins (round(extreu-x-2p f aresta))
	(round(extreu-y-2p f aresta)))
)
;Extreu la component x del primer punt d'una aresta de la figura:
(defun extreu-x-1p (f aresta)
	(car (car (multiplica-matrius 
	(list (snoc 1 (agafa-punt (get f 'punts) (car aresta))))
	(get f 'matriu))))
)
;Extreu la component y del primer punt d'una aresta de la figura:
(defun extreu-y-1p (f aresta)
	(cadr (car (multiplica-matrius 
	(list (snoc 1 (agafa-punt (get f 'punts) (car aresta))))
	(get f 'matriu))))
)
;Extreu la component x del primer punt d'una aresta de la figura:
(defun extreu-x-2p (f aresta)
	(car (car (multiplica-matrius 
	(list (snoc 1 (agafa-punt (get f 'punts) (cadr aresta))))
	(get f 'matriu))))
)
;Extreu la component y del primer punt d'una aresta de la figura:
(defun extreu-y-2p (f aresta)
	(cadr (car (multiplica-matrius 
	(list (snoc 1 (agafa-punt (get f 'punts) (cadr aresta))))
	(get f 'matriu))))
)
;Afegeix l'element 'e' al final de la llista 'l':
(defun snoc (e l)
	(cond ((null l) (cons e l))
		  (T (cons (car l) (snoc e (cdr l))))
	)
)
;Torna el punt 'n'-èssim d'una llista de punts donada:
(defun agafa-punt (llista-punts n)
	(cond ((= n 1) (car llista-punts))
		  (T (agafa-punt (cdr llista-punts) (- n 1)))
	)
)


;(pinta-figures): pinta totes les figures de la llista “figures”
(defun pinta-figures ()
	(pinta-totes-figures figures)
)
(defun pinta-totes-figures (figures)
	(cond ((null figures) (print 'totes_les_figures_pintades))
		  ((pinta-figura (car figures))
		  (pinta-totes-figures (cdr figures)))
	)
)


;(trasllada-figura f x y z): trasllada la figura f, a unitats a
;l’eix x, b unitats a l’eix y i c unitats a l’eix z
(defun trasllada-figura (f x y z)
	(putprop f (multiplica-matrius (get f 'matriu)
	(list '(1 0 0 0) '(0 1 0 0) '(0 0 1 0) (list x y z 1)))
	'matriu)
)


;(rota-figura f x y z): rota la figura f, a unitats respecte a l’eix
;x, b unitats respecte a l’eix y ic unitats respecte a l’eix z
(defun rota-figura (f x y z)
	(putprop f (multiplica-matrius (get f 'matriu)
	(list '(1 0 0 0) (list 0 (cos x) (sin x) 0)
	(list 0 (- 0 (sin  x)) (cos x) 0) '(0 0 0 1)))
	'matriu)
	(putprop f (multiplica-matrius (get f 'matriu)
	(list (list (cos y) 0 (- 0 (sin y)) 0) '(0 1 0 0)
	(list (sin y) 0 (cos y) 0) '(0 0 0 1)))
	'matriu)
	(putprop f (multiplica-matrius (get f 'matriu)
	(list (list (cos z) (sin z) 0 0)
	(list (- 0 (cos z))(cos z) 0 0) '(0 0 1 0) '(0 0 0 1)))
	'matriu)
)


;(escala-figura f x y z): escala la figura f, un factor a respecte
;a l’eix x, un factor b respecte a l’eix y i un factor c respecte a
;l’eix z
(defun escala-figura (f x y z)
	(putprop f (multiplica-matrius (get f 'matriu)
	(list (list x 0 0 0) (list 0 y 0 0) (list 0 0 z 0) '(0 0 0 1)))
	'matriu)
)


;(repeteixn n f): repeteix n vegades la funció f
(defun repeteixn (n f)
    (cond ((= n 0) nil)
          (t (eval f) (repeteixn (- n 1) f))
	)
)


;(inicia-figura f): posa la figura f a la seva posició inicial
;(matriu identitat a la transformació)
(defun inicia-figura (f)
	(putprop f m-identitat 'matriu)
)



;==============================================================
;================== MULTIPLICACIÓ DE MATRIUS ==================
;==============================================================

;Multiplica les matrius 'n' i 'm':
(defun multiplica-matrius (m n)
	(cond ((NULL m) nil)
		  ((NULL n) nil)
		  (t (cons (fila-columnes (car m) (transposada n '1))
			 (multiplica-matrius (cdr m) n))
		  )
	)
)

;Construeix la columna 'n' de la matriu 'm' emprant la matriu transposada:
(defun transposada (m n)
	(cond ((NULL m) nil)
		  ((NULL (obtenirn n m)) nil)
		  (t (cons (nsimes-files m n) (transposada  m (+ n 1))))
	)
)

;Extreu una llista amb el nombre de files especificat per 'n':
(defun nsimes-files(m n)
	(cond ((NULL m) nil)
		  ((NULL (car m)) nil)
		  (t (cons (obtenirn n (car m)) (nsimes-files (cdr m) n)))
	)
)

;Construeix l'estructura per multiplicar la fila 'f' per 'n' columnes:
(defun fila-columnes(f n)
	(cond ((NULL n) nil)
		((NULL f) nil)
		(t (cons (fila-columna f (car n)) (fila-columnes f (cdr n))))
	)
)

;Multiplica la fila 'f' per la columna 'c':
(defun fila-columna(f c)
	(cond ((NULL f) '0)
		  ((NULL c) '0)
		  (t (+ (* (car f) (car c)) (fila-columna (cdr f) (cdr c))))
	)
)

;Obté l'element 'n'-èssim de la llista 'l':
(defun obtenirn (n l)
	(cond ((= n 0) nil)
		  ((= n 1) (car l))
		  (t (obtenirn (- n 1) (cdr l)))
	)
)