;
; Tortuga
;   Propietats:
;      - x  -> coordenada x absoluta, de 0 a 639
;      - y  -> coordenada y absoluta, de 0 a 374
;      - angle -> angle absolut respecte a l'horitzontal
;                 positiu = al revés del rellotge
;   Comportament:
;     baixa-llapis -> si es mou pinta
;     puja-llapis -> si es mou no pinta
;     inicia -> posa tortuga al centre de la pantalla
;               l'angle a 0, baixa el llapis per pintar, i
;               posa el fons negre i la lletra vermella
;     pinta -> dibuixa un segment sumant (x,y) a la posició actual
;     pinta-fins -> dibuixa un segment de la posició actual fins a (x,y)
;              
;     pinta-fins dibuixa un segment fins a la posicio (x,y)
;     mou   -> suma (x,y) a la posició actual i hi va sense
;              pintar
;     res   -> la tortuga no fa res
;     
;     bota  -> situa la tortuga a la posició absoluta (x,y)
;
;     angle -> situa la tortua amb un angle absolut respecte 
;              a l'horitzontal
;              positiu = al revés del rellotge
;     posicio -> torna una llista amb la posicio i angle de la tortuga
;               (x y angle)
;     endavant ->  va n pixels cap endavant pintant si el llapis
;                  esta baixat
;     endarrere -> va n pixels cap endarrere pintant si el llapis
;                  esta baixat
;     gira-dreta-> gira n graus a la dreta
;     gira-esquerra-> gira n graus a la dreta
;
;     grafics -> inicia la tortuga i llegeix repetitivament 
;                    comandes a la linia
;                    superior de la pantall i pinta a la resta
;                    les operacions de la tortuga
;
;     entrada -> torna passar al mode grafic en cas d'error



(defun baixa-llapis () (putprop 'tortuga t 'llapis))

(defun puja-llapis ()  (putprop 'tortuga nil 'llapis))

(defun inicia ()   
                   (putprop 'tortuga 0 'angle) 
                   (putprop 'tortuga t 'llapis)
                   (putprop 'tortuga 320 'x)
                   (putprop 'tortuga 187 'y)
                   (bota (get 'tortuga 'x) (get 'tortuga 'y))
                   (color 12)  ; n és de 8 bits, els 3 menys significatius són RGB pel fons
                               ; el 4 bit es de brillo
                               ; els tres següents són el color del text 
                   (cls))

(defun pinta (x y)  
                   (putprop 'tortuga (+ (get 'tortuga 'x) x) 'x) 
                   (putprop 'tortuga (+ (get 'tortuga 'y) y) 'y)
                   (drawrel x y))

(defun pinta-fins (x y)  
                   (putprop 'tortuga x 'x) 
                   (putprop 'tortuga y 'y)
                   (draw x y))

(defun mou (x y)   
                   (putprop 'tortuga (+ (get 'tortuga 'x) x) 'x) 
                   (putprop 'tortuga (+ (get 'tortuga 'y) y) 'y) 
                   (moverel x y))

(defun res ())

(defun bota (x y)  
                   (putprop 'tortuga x 'x) 
                   (putprop 'tortuga y 'y) 
                   (move x y))

(defun angle (n)   (putprop 'tortuga n 'angle))

(defun posicio ()  (list (get 'tortuga 'x) (get 'tortuga 'y) 
                   (get 'tortuga 'angle)))

(defun endavant (n)
  (cond 
    ((get 'tortuga 'llapis)
      (pinta (realpart 
                   (round (* n 
                          (cos (radians (get 'tortuga 'angle))))))
      (realpart (round (* n 
                          (sin (radians (get 'tortuga 'angle))))))))
    (t
      (mou (realpart 
                   (round (* n 
                          (cos (radians (get 'tortuga 'angle))))))
      (realpart (round (* n 
                          (sin (radians (get 'tortuga 'angle))))))))
  ))      


(defun endarrere (n) (gira-dreta 180) (endavant n) 
                     (gira-esquerra 180))    


(defun gira-dreta (n)
    (putprop 'tortuga 
             (mod (- (get 'tortuga 'angle) n) 360)  'angle ))

(defun gira-esquerra (n)
    (putprop 'tortuga 
             (mod (+ (get 'tortuga 'angle) n) 360) 'angle ))

(defun radians (n)
    (/ (* (* 2 pi) n) 360))




(defun entrada ()  (cls) (repetir (eval (read))))

(defun grafics ()
   (inicia)
   (entrada)
)

(defun repetir (x) (goto-xy '0 '0) (cleol) (repetir (eval (read))))


