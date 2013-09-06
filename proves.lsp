(load 'practica_lisp)
(load 'tortuga)

(crea-figura 'cub1 'cub '(255 0 0))
(escala-figura 'cub1 150 150 150)
(rota-figura 'cub1 45 45 45)
(trasllada-figura 'cub1 100 100 0)

(crea-figura 'prisma1 'prisma '(255 0 0))
(escala-figura 'prisma1 150 150 150)
(rota-figura 'prisma1 45 45 45)
(trasllada-figura 'prisma1 300 100 0)

(crea-figura 'octaedre1 'octaedre '(255 0 0))
(escala-figura 'octaedre1 150 150 150)
(rota-figura 'octaedre1 45 45 45)
(trasllada-figura 'octaedre1 500 100 0)

(grafics)

(pinta-figura 'cub1)
(pinta-figura 'prisma1)
(pinta-figura 'octaedre1)