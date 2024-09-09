;"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;                                                 Tarea #2                                           ;
;                                        Paradigmas de programación                                  ;
;                                 Desarrollo de Juego Tic Tac Toe en Racket                          ;
;                                              Logica del juego                                      ;
;                                               Programadores:                                       ;
;                                               Asly Barahona                                        ;
;                                              Joaquín Ramirez                                       ;
;                                              Fabián Gutierrez                                      ;
;                                               Emmanuel Calvo                                       ;
;"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

#lang racket

(provide (all-defined-out))


; Genera una matriz para el juego
(define (genMat filas cols)
  (cond
    ((or (>= 2 filas) (>= 2 cols)
         (<= 11 filas) (<= 11 cols)) #f)
    (else (genMatAux filas cols cols '() '()))))

(define (genMatAux filas cols colsIni fila mat)
  (cond
    ((zero? filas) mat)
    ((zero? cols) (genMatAux (- filas 1) colsIni colsIni '() (append mat (list fila))))
    (else (genMatAux filas (- cols 1) colsIni (append fila (list '-)) mat))))

; Obtiene un elemento en una posición dada de una lista
(define (getEn indice lista)
  (cond
    ((or (> indice (length lista)) (<= indice 0)) #f) ; Valida índices
    ((equal? indice 1) (car lista))
    (else (getEn (- indice 1) (cdr lista)))))

; Obtiene un elemento en una posición dada de una matriz
(define (getEnMat i j mat)
  (getEn j (getEn i mat)))

; Establece un elemento en una posición dada de una lista
(define (setEn indice elemento lista)
  (cond
    ((or (> indice (length lista)) (<= indice 0)) #f) ; Valida índices
    ((equal? indice 1) (append (list elemento) (cdr lista)))
    (else (append (list (car lista)) (setEn (- indice 1) elemento (cdr lista))))))

; Establece un elemento en una posición dada de una matriz
(define (setEnMat i j elemento mat)
  (cond
    ((or (> i (length mat)) (<= i 0) (> j (length (car mat))) (<= j 0)) #f) ; Valida índices
    (else (setEnMatAux i j elemento mat))))
                  
(define (setEnMatAux i j elemento mat)
  (cond
    ((equal? i 1) (append (list (setEn j elemento (car mat))) (cdr mat)))
    (else (append (list (car mat)) (setEnMatAux (- i 1) j elemento (cdr mat))))))

; Obtiene una columna dada un índice
(define (getCol j mat)
  (cond
    ((or (> j (length (car mat))) (<= j 0)) #f) ; Valida índices
    (else (getColAux j (length mat) mat))))

(define (getColAux j filas mat)
  (cond
    ((zero? filas) '())
    (else (append (getColAux j (- filas 1) mat) (list (getEnMat filas j mat))))))

; Obtiene una fila dada un índice
(define (getFila i mat)
  (cond
    ((or (> i (length mat)) (<= i 0)) #f) ; Valida índices
    (else (getFilaAux i (length (car mat)) mat))))

(define (getFilaAux i cols mat)
  (cond
    ((zero? cols) '())
    (else (append (getFilaAux i (- cols 1) mat) (list (getEnMat i cols mat))))))

; Obtiene las diagonales de una matriz
(define (getDiags mat)
  (cond
    ((equal? (length mat) (length (car mat))) (getDiagCuad (length mat) mat))
    ((> (length (car mat)) (length mat)) (getDiagNoC 1 1 1 (length mat) (length (car mat)) (- (length (car mat)) (- (length mat) 1)) '() '() mat))
    (else (getDiagNoC 1 1 1 (length mat) (length (car mat)) (- (length mat) (- (length (car mat)) 1)) '() '() mat))))

; Obtiene la diagonal de una matriz cuadrada
(define (getDiagCuad filas mat)
  (cond
    ((zero? filas) '())
    (else (append (getDiagCuad (- filas 1) mat) (list (getEnMat filas filas mat))))))

; Obtiene las diagonales de una matriz no cuadrada
(define (getDiagNoC i j indexdiag filas cols numdiags pardiag totdiag mat)
  (cond
    ((zero? numdiags) totdiag)
    ((> cols filas)
     (cond
       ((equal? j (+ indexdiag filas)) (getDiagNoC 1 (+ indexdiag 1) (+ indexdiag 1) filas cols (- numdiags 1) '() (append totdiag (list pardiag)) mat))
       (else (getDiagNoC (+ i 1) (+ j 1) indexdiag filas cols numdiags (append pardiag (list (getEnMat i j mat))) totdiag mat))))
    ((< cols filas)
     (cond
       ((equal? i (+ indexdiag cols)) (getDiagNoC (+ indexdiag 1) 1 (+ indexdiag 1) filas cols (- numdiags 1) '() (append totdiag (list pardiag)) mat))
       (else (getDiagNoC (+ i 1) (+ j 1) indexdiag filas cols numdiags (append pardiag (list (getEnMat i j mat))) totdiag mat))))))

; Obtiene las diagonales inversas de una matriz
(define (getDiagsInv mat)
  (cond
    ((equal? (length mat) (length (car mat))) (getDiagInvCuad (length mat) 1 mat))
    ((> (length (car mat)) (length mat)) (getDiagInvNoC (length mat) 1 1 (length mat) (length (car mat)) (- (length (car mat)) (- (length mat) 1)) '() '() mat))
    (else (getDiagInvNoC 1 (length (car mat)) 1 (length mat) (length (car mat)) (- (length mat) (- (length (car mat)) 1)) '() '() mat))))  

; Obtiene la diagonal inversa de una matriz cuadrada
(define (getDiagInvCuad filas cols mat)
  (cond
    ((zero? filas) '())
    (else (append (getDiagInvCuad (- filas 1) (+ cols 1) mat) (list (getEnMat filas cols mat))))))

; Obtiene las diagonales inversas de una matriz no cuadrada
(define (getDiagInvNoC i j indexdiag filas cols numdiags pardiag totdiag mat)
  (cond
    ((zero? numdiags) totdiag)
    ((> cols filas)
     (cond
       ((equal? i filas) (getDiagInvNoC 1 (+ indexdiag 1) (+ indexdiag 1) filas cols (- numdiags 1) '() (append totdiag (list pardiag)) mat))
       (else (getDiagInvNoC (+ i 1) (- j 1) indexdiag filas cols numdiags (append pardiag (list (getEnMat i j mat))) totdiag mat))))
    ((< cols filas)
     (cond
       ((equal? j 1) (getDiagInvNoC (+ indexdiag 1) cols (+ indexdiag 1) filas cols (- numdiags 1) '() (append totdiag (list pardiag)) mat))
       (else (getDiagInvNoC (+ i 1) (- j 1) indexdiag filas cols numdiags (append pardiag (list (getEnMat i j mat))) totdiag mat))))))

; Verifica si hay una victoria en una fila
(define (checkVictFila jugador mat)
  (checkVictFilaAux jugador (length mat) mat))

(define (checkVictFilaAux jugador filas mat)
  (cond
    ((zero? filas) #f)
    ((checkLinea jugador (getFila filas mat)) #t)
    (else (checkVictFilaAux jugador (- filas 1) mat))))

; Verifica si hay una victoria en una columna
(define (checkVictCol jugador mat)
  (checkVictColAux jugador (length (car mat)) mat))

(define (checkVictColAux jugador cols mat)
  (cond
    ((zero? cols) #f)
    ((checkLinea jugador (getCol cols mat)) #t)
    (else (checkVictColAux jugador (- cols 1) mat))))

; Verifica si hay una victoria en una diagonal
(define (checkVictDiag jugador mat)
  (cond
    ((equal? (length mat) (length (car mat)))
     (cond
       ((checkLinea jugador (getDiags mat)) #t)
       ((checkLinea jugador (getDiagsInv mat)) #t)
       (else #f)))
    (else
     (cond
       ((checkVictDiagAux jugador (getDiags mat)) #t)
       ((checkVictDiagAux jugador (getDiagsInv mat)) #t)
       (else #f)))))

(define (checkVictDiagAux jugador diags)
  (cond
    ((null? diags) #f)
    ((checkLinea jugador (car diags)) #t)
    (else (checkVictDiagAux jugador (cdr diags)))))

    
; Verifica si todos los elementos en una línea son iguales al token del jugador  
(define (checkLinea jugador linea)
  (cond
    ((null? linea) #t)
    ((not (equal? (car linea) jugador)) #f)
    (else (checkLinea jugador (cdr linea)))))


; Cuenta cuántos elementos diferentes al token del jugador están en una línea
(define (contarLineaDif jugador linea)
  (cond
    ((null? linea) 0)
    ((equal? jugador 'X)
     (cond
       ((equal? (car linea) 'O) (+ 1 (contarLineaDif jugador (cdr linea))))
       (else (+ 0 (contarLineaDif jugador (cdr linea))))))
    (else
     (cond
       ((equal? (car linea) 'X) (+ 1 (contarLineaDif jugador (cdr linea))))
       (else (+ 0 (contarLineaDif jugador (cdr linea))))))))

; 4.2.1. Conjunto de candidatos: Obtiene las posiciones disponibles en la matriz
(define (candidatos mat)
  (candidatosAux (length mat) (length (car mat)) 1 1 '() mat))

(define (candidatosAux filas cols i j poslist mat)
  (cond
    ((equal? i (+ filas 1)) poslist)
    ((equal? j (+ cols 1)) (candidatosAux filas cols (+ i 1) 1 poslist mat))
    ((equal? '- (getEnMat i j mat)) (candidatosAux filas cols i (+ j 1) (append poslist (list (list i j))) mat))
    (else (candidatosAux filas cols i (+ j 1) poslist mat))))

; Obtiene las posiciones vacías en una línea
(define (getPosVacias linea mat)
  (getPosVaciasAux 1 (getEn 1 linea) (getEn 2 linea) (getEn 4 linea) (length (getEn 4 linea)) (length mat) (length (car mat)) '()))

(define (getPosVaciasAux aux1 tipo num linea lenlinea filas cols resultado)
  (cond
    ((zero? lenlinea) resultado)
    ((equal? tipo 'row)
     (cond
       ((equal? (getEn lenlinea linea) '-) (getPosVaciasAux aux1 tipo num linea (- lenlinea 1) filas cols (append (list (list num lenlinea)) resultado)))
       (else (getPosVaciasAux aux1 tipo num linea (- lenlinea 1) filas cols resultado))))
    ((equal? tipo 'column)
     (cond
       ((equal? (getEn lenlinea linea) '-) (getPosVaciasAux aux1 tipo num linea (- lenlinea 1) filas cols (append (list (list lenlinea num)) resultado)))
       (else (getPosVaciasAux aux1 tipo num linea (- lenlinea 1) filas cols resultado))))
    ((equal? tipo 'diagonal)
     (cond
       ((and (equal? (getEn lenlinea linea) '-) (> filas cols))
        (getPosVaciasAux (+ aux1 1) tipo num linea (- lenlinea 1) (- filas 1) (- cols 1) (append (list (list aux1 lenlinea)) resultado)))
       ((and (equal? (getEn lenlinea linea) '-) (equal? filas cols))
        (getPosVaciasAux aux1 tipo num linea (- lenlinea 1) (- filas 1) (- cols 1) (append (list (list lenlinea lenlinea)) resultado)))
       (else (getPosVaciasAux aux1 tipo num linea (- lenlinea 1) (- filas 1) (- cols 1) resultado))))
    ((equal? tipo 'invdiagonal)
     (cond
       ((and (equal? (getEn lenlinea linea) '-) (> filas cols))
        (getPosVaciasAux (+ aux1 1) tipo num linea (- lenlinea 1) (- filas 1) (- cols 1) (append (list (list (+ lenlinea (- num 1)) aux1)) resultado)))
       ((and (equal? (getEn lenlinea linea) '-) (equal? filas cols))
        (getPosVaciasAux (+ aux1 1) tipo num linea (- lenlinea 1) (- filas 1) (- cols 1) (append (list (list lenlinea aux1)) resultado)))         
       ((and (equal? (getEn lenlinea linea) '-) (> cols filas))
        (getPosVaciasAux (+ aux1 1) tipo (+ num 1) linea (- lenlinea 1) (- filas 1) (- cols 1) (append (list (list (+ lenlinea (- num 1)) aux1)) resultado)))
       (else
        (cond
          ((or (> filas cols) (equal? filas cols)) (getPosVaciasAux (+ aux1 1) tipo num linea (- lenlinea 1) (- filas 1) (- cols 1) resultado))
          (else (getPosVaciasAux (+ aux1 1) tipo (+ num 1) linea (- lenlinea 1) (- filas 1) (- cols 1) resultado))))))))

; 4.2.2. Función de selección: Obtiene la línea más prometedora para ganar
(define (seleccion jugador mat)
  (seleccionAux jugador mat 11 '()))

(define (seleccionAux jugador mat mayor resultado)
  (cond
    ((< (getEn 3 (getFilaOptima jugador 1 11 (length mat) mat '())) mayor)
     (seleccionAux jugador mat (getEn 3 (getFilaOptima jugador 1 11 (length mat) mat '())) (getFilaOptima jugador 1 11 (length mat) mat '())))
    ((< (getEn 3 (getColOptima jugador 1 11 (length (car mat)) mat '())) mayor)
     (seleccionAux jugador mat (getEn 3 (getColOptima jugador 1 11 (length (car mat)) mat '())) (getColOptima jugador 1 11 (length (car mat)) mat '())))
    ((< (getEn 3 (getDiagOptima jugador 1 11 (length (getDiags mat)) (getDiags mat) '())) mayor)
     (seleccionAux jugador mat (getEn 3 (getDiagOptima jugador 1 11 (length (getDiags mat)) (getDiags mat) '())) (getDiagOptima jugador 1 11 (length (getDiags mat)) (getDiags mat) '())))
    ((< (getEn 3 (getDiagInvOptima jugador 1 11 (length (getDiagsInv mat)) (getDiagsInv mat) '())) mayor)
     (seleccionAux jugador mat (getEn 3 (getDiagInvOptima jugador 1 11 (length (getDiagsInv mat)) (getDiagsInv mat) '())) (getDiagInvOptima jugador 1 11 (length (getDiagsInv mat)) (getDiagsInv mat) '())))
    (else
     (cond
       ((null? resultado) resultado)
       (else (append resultado (list (getPosVacias resultado mat))))))))

; Obtiene la fila más prometedora para ganar
(define (getFilaOptima jugador mejorIndice mejor filas mat resultado)
  (cond
    ((zero? filas) (append (list 'row) (list mejorIndice mejor) (list resultado)))
    ((and (< (objetivo jugador (getEn filas mat)) mejor) (zero? (contarLineaDif jugador (getEn filas mat))))
     (getFilaOptima jugador filas (objetivo jugador (getEn filas mat)) (- filas 1) mat (getEn filas mat)))
    (else (getFilaOptima jugador mejorIndice mejor (- filas 1) mat resultado))))

; Obtiene la columna más prometedora para ganar
(define (getColOptima jugador mejorIndice mejor cols mat resultado)
  (cond
    ((zero? cols) (append (list 'column) (list mejorIndice mejor) (list resultado)))
    ((and (< (objetivo jugador (getCol cols mat)) mejor) (zero? (contarLineaDif jugador (getCol cols mat))))
     (getColOptima jugador cols (objetivo jugador (getCol cols mat)) (- cols 1) mat (getCol cols mat)))
    (else (getColOptima jugador mejorIndice mejor (- cols 1) mat resultado))))

; Obtiene la diagonal más prometedora para ganar
(define (getDiagOptima jugador mejorIndice mejor numDiags diags resultado)
  (cond
    ((not (list? (car diags)))
     (cond
       ((zero? (contarLineaDif jugador diags)) (append (list 'diagonal) (list 1 (objetivo jugador diags) diags)))
       (else (append (list 'diagonal) (list 1 11 '())))))
    ((zero? numDiags) (append (list 'diagonal) (list mejorIndice mejor) (list resultado)))
    ((and (< (objetivo jugador (getEn numDiags diags)) mejor) (zero? (contarLineaDif jugador (getEn numDiags diags))))
     (getDiagOptima jugador numDiags (objetivo jugador (getEn numDiags diags)) (- numDiags 1) diags (getEn numDiags diags)))
    (else (getDiagOptima jugador mejorIndice mejor (- numDiags 1) diags resultado))))

; Obtiene la diagonal inversa más prometedora para ganar
(define (getDiagInvOptima jugador mejorIndice mejor numDiagsInv diagsInv resultado)
  (cond
    ((not (list? (car diagsInv)))
     (cond
       ((zero? (contarLineaDif jugador diagsInv)) (append (list 'invdiagonal) (list 1 (objetivo jugador diagsInv) diagsInv)))
       (else (append (list 'invdiagonal) (list 1 11 '())))))
    ((zero? numDiagsInv) (append (list 'invdiagonal) (list mejorIndice mejor) (list resultado)))
    ((and (< (objetivo jugador (getEn numDiagsInv diagsInv)) mejor) (zero? (contarLineaDif jugador (getEn numDiagsInv diagsInv))))
     (getDiagInvOptima jugador numDiagsInv (objetivo jugador (getEn numDiagsInv diagsInv)) (- numDiagsInv 1) diagsInv (getEn numDiagsInv diagsInv)))
    (else (getDiagInvOptima jugador mejorIndice mejor (- numDiagsInv 1) diagsInv resultado))))

; 4.2.3. Función de viabilidad: Verifica la viabilidad de colocar un token en una posición
(define (viabilidad jugador1 jugador2 mat)
  (viabilidadAux jugador1 jugador2 (seleccion jugador1 mat) (seleccion jugador2 mat) (candidatos mat) mat))

(define (viabilidadAux jugador1 jugador2 jugada1 jugada2 posDisp mat)
  (cond
    ((and (null? jugada1) (null? jugada2)) "fin del juego")
    ((and (not (null? jugada2)) (equal? (length (getEn 5 jugada2)) 1)) (getEn 1 (getEn 5 jugada2)))
    ((null? jugada1) (getEn 1 (getEn 5 jugada2)))
    ((equal? (length (getEn 5 jugada1)) 1) (getEn 1 (getEn 5 jugada1)))
    (else
     (cond
       ((null? jugada2) (getEn 1 (getEn 5 jugada1)))
       ((not (null? (interseccion (getEn 5 jugada1) (getEn 5 jugada2)))) (car (interseccion (getEn 5 jugada1) (getEn 5 jugada2))))
       (else (getEn 1 (getEn 5 jugada1)))))))

; Coloca el token en la posición más viable
(define (ponerToken jugador1 jugador2 mat)
  (cond
    ((list? (viabilidad jugador1 jugador2 mat))
     (print (viabilidad jugador1 jugador2 mat))
     (newline)
     (setEnMat (car (viabilidad jugador1 jugador2 mat)) (cadr (viabilidad jugador1 jugador2 mat)) jugador2 mat))
    (else (append (list "No hay movimientos para ganar") mat))))

; Verifica si un elemento está en una lista
(define (miembro ele lista)
  (cond
    ((null? lista) #f)
    ((equal? (car lista) ele) #t)
    (else (miembro ele (cdr lista)))))

; Devuelve los elementos de la intersección entre dos listas
(define (interseccion linea1 linea2)
  (cond
    ((null? linea1) '())
    ((miembro (car linea1) linea2) (append (list (car linea1)) (interseccion (cdr linea1) linea2)))
    (else (interseccion (cdr linea1) linea2))))

; 4.2.4. Función objetivo: Cuenta cuántos elementos faltan para llenar la línea
(define (objetivo jugador linea)
  (contarLineaAux jugador linea (length linea) 0))

(define (contarLineaAux jugador linea longitud resultado)
  (cond
    ((null? linea) (- longitud resultado))
    ((equal? (car linea) jugador) (contarLineaAux jugador (cdr linea) longitud (+ resultado 1)))
    (else (contarLineaAux jugador (cdr linea) longitud resultado))))

; 4.2.5. Función de solución: Verifica si hay una victoria
(define (solucion jugador mat)
  (or (checkVictFila jugador mat)
      (checkVictCol jugador mat)
      (checkVictDiag jugador mat)))

;Función que imprime la matriz
(define (printMat tabla condicion)
  (for ([i (length tabla)])
    (for ([j (length (car tabla))])
      (printf "~a\t" (list-ref (list-ref tabla i) j)))
    (newline))
  (newline)
  condicion)