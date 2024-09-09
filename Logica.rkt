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

#lang racket/gui

(provide (all-defined-out))

;Funcion que genera una matriz de numrows x numcols
(define (genMat filas cols)
  (cond
    (( or (>= 2 filas) (>= 2 cols)
          (<= 11 filas) (<= 11 cols))#f)
    (else (genMatAux filas cols cols '() '() ))))

(define (genMatAux filas cols colsIni fila mat)
  (cond
    ((zero? filas) mat)
    ((zero? cols) (genMatAux (- filas 1) colsIni colsIni '() (append mat (list fila))))
    (else (genMatAux filas (- cols 1) colsIni (append fila (list '-)) mat))))


;Funcion para obtener un elemento en una posicion dada en una lista
(define (getEn indice lista)
  (cond
    (( or (> indice (length lista)) (<= indice 0)) #f) ;valida indices
    ((equal? indice 1) (car lista))
    (else (getEn (- indice 1) (cdr lista)))))

;Funcion para obtener un elemento en una posicion dada en una matriz
(define (getEnMat i j mat)
  (getEn j (getEn i mat)))

;Funcion para establecer un elemento en una posicion dada en una lista
(define (setEn indice elemento lista)
  (cond
    (( or (> indice (length lista)) (<= indice 0)) #f) ;valida indices
    ((equal? indice 1) (append (list elemento) (cdr lista)))
    (else (append (list (car lista)) (setEn (- indice 1) elemento (cdr lista))))))

;Funciones para establecer un elemento en una posicion dada en una matriz
(define (setEnMat i j elemento mat)
  (cond
    (( or (> i (length mat)) (<= i 0) (> j (length (car mat))) (<= j 0)) #f) ;validates indexes
    (else (setEnMatAux i j elemento mat))))
                  
(define (setEnMatAux i j elemento mat)
  (cond
    ((equal? i 1) (append (list (setEn j elemento (car mat))) (cdr mat)))
    (else (append (list (car mat)) (setEnMatAux (- i 1) j elemento (cdr mat))))))

;Funcion que devuelve una columna dada un indice
(define (getCol j mat)
  (cond
    (( or (> j (length (car mat))) (<= j 0)) #f) ;valida indices
    (else (getColAux j (length mat) mat))))

(define (getColAux j filas mat)
  (cond
    ((zero? filas) '())
    (else (append  (getColAux j (- filas 1) mat) (list (getEnMat filas j mat))))))

;Funcion que devuelve una fila dada un indice
(define (getFila i mat)
  (cond
    (( or (> i (length mat)) (<= i 0)) #f) ;valida indices
    (else (getFilaAux i (length (car mat)) mat))))

(define (getFilaAux i cols mat)
  (cond
    ((zero? cols) '())
    (else (append  (getFilaAux i (- cols 1) mat) (list (getEnMat i cols mat))))))

;Funcion que obtiene una diagonal en una matriz cuadrada o no cuadrada
(define (getDiags mat)
  (cond
    ((equal? (length mat) (length (car mat))) (getDiagCuad (length mat) mat))
    ((>  (length (car mat)) (length mat)) (getDiagNoC  1 1 1 (length mat) (length (car mat)) (- (length (car mat)) (- (length mat) 1)) '() '() mat))
    (else (getDiagNoC  1 1 1 (length mat) (length (car mat)) (- (length mat) (- (length (car mat))  1)) '() '() mat))))

;Funcion que obtiene la diagonal de una matriz cuadrada
(define (getDiagCuad filas mat)
  (cond
    ((zero? filas) '())
    (else (append (getDiagCuad (- filas 1) mat) (list (getEnMat filas filas mat))))))

;Funcion que obtiene las diagonales de una matriz no cuadrada
(define (getDiagNoC  i j indexdiag filas cols numdiags pardiag totdiag mat)
  (cond
    ((zero? numdiags) totdiag)
    ((> cols filas)
     (cond
       ((equal? j (+ indexdiag filas)) (getDiagNoC  1 (+ indexdiag 1) (+ indexdiag 1) filas cols (- numdiags 1) '() (append totdiag (list pardiag)) mat))
       (else (getDiagNoC  (+ i 1) (+ j 1) indexdiag filas cols numdiags (append pardiag (list (getEnMat i j mat))) totdiag mat))))
    ((< cols filas)
     (cond
       ((equal? i (+ indexdiag cols)) (getDiagNoC  (+ indexdiag 1) 1 (+ indexdiag 1) filas cols (- numdiags 1) '() (append totdiag (list pardiag)) mat))
       (else (getDiagNoC  (+ i 1) (+ j 1) indexdiag filas cols numdiags (append pardiag (list (getEnMat i j mat))) totdiag mat))
       ))))

;Funcion que obtiene una diagonal inversa en una matriz cuadrada o no cuadrada
(define (getDiagsInv mat)
  (cond
    ((equal? (length mat) (length (car mat))) (getDiagInvCuad (length mat) 1 mat))
    ((>  (length (car mat)) (length mat)) (getDiagInvNoC (length mat) 1 1 (length mat) (length (car mat)) (- (length (car mat)) (- (length mat) 1)) '() '() mat))
    (else (getDiagInvNoC 1 (length (car mat)) 1 (length mat) (length (car mat)) (- (length mat) (- (length (car mat))  1)) '() '() mat))))  

;Funcion que obtiene la diagonal inversa de una matriz cuadrada
(define (getDiagInvCuad filas cols mat)
  (cond
    ((zero? filas) '())
    (else (append (getDiagInvCuad (- filas 1) (+ cols 1) mat) (list (getEnMat filas cols mat))))))

;Funcion que obtiene las diagonales inversas de una matriz no cuadrada
(define (getDiagInvNoC i j indexdiag filas cols numdiags pardiag totdiag mat)
  (cond
    ((zero? numdiags) totdiag)
    ((> cols filas)
     (cond
       ((equal? j (+ indexdiag filas)) (getDiagInvNoC filas (+ indexdiag 1) (+ indexdiag 1) filas cols (- numdiags 1) '() (append  totdiag (list pardiag)) mat))
       (else (getDiagInvNoC (- i 1) (+ j 1) indexdiag filas cols numdiags (append (list (getEnMat i j mat)) pardiag ) totdiag mat))))
    ((< cols filas)
     (cond
       ((equal? i (+ indexdiag cols)) (getDiagInvNoC (+ indexdiag 1) cols (+ indexdiag 1) filas cols (- numdiags 1) '() (append  totdiag (list pardiag) ) mat))
       (else (getDiagInvNoC (+ i 1) (- j 1) indexdiag filas cols numdiags (append  pardiag (list (getEnMat i j mat))) totdiag mat))))))
        
;Funciones que verifican si hay una victoria en una fila

(define (solucionFila turno mat)
  (solucionFilaAux turno (length mat) mat ))

(define (solucionFilaAux turno filas mat)
  (cond
    ((zero? filas) #f)
    ((checkFila turno (getFila filas mat)) #t)
    (else (solucionFilaAux turno (- filas 1) mat))))

;Funciones que verifican si hay una victoria en una columna
(define (solucionCol turno mat)
  (solucionColAux turno (length (car mat)) mat ))

(define (solucionColAux turno cols mat)
  (cond
    ((zero? cols) #f)
    ((checkFila turno (getCol cols mat)) #t)
    (else (solucionColAux turno (- cols 1) mat))))

;Funciones que verifican si hay una victoria en una diagonal
(define (solucionDiag turno mat)
  (cond
    ((equal? (length mat) (length (car mat)))
     (cond
       ((checkFila turno (getDiags mat)) #t)
       ((checkFila turno (getDiagsInv mat)) #t)
       (else #f)))
    (else
     (cond
       ((solucionDiagAux turno (getDiags mat)) #t)
       ((solucionDiagAux turno (getDiagsInv mat)) #t)
       (else #f)))))

(define (solucionDiagAux turno diags)
  (cond
    ((null? diags) #f)
    ((checkFila turno (car diags)) #t)
    (else (solucionDiagAux turno (cdr diags)))))
    
;Funcion que verifica si todos los elementos en una fila son iguales al token del jugador  
(define (checkFila turno fila)
  (cond
    ((null? fila) #t)
    ((not (equal? (car fila) turno)) #f)
    (else (checkFila turno (cdr fila)))))

  

;Funcion que cuenta cuantos elementos diferentes de un token estan en una fila
(define (contarLineaDif turno linea)
  (cond
    ((null? linea) 0)
    ((equal? turno 'X)
     (cond
       ((equal? (car linea) 'O) (+ 1 (contarLineaDif turno (cdr linea))))
       (else (+ 0 (contarLineaDif turno (cdr linea))))))
    (else
     (cond
       ((equal? (car linea) 'X) (+ 1 (contarLineaDif turno (cdr linea))))
       (else (+ 0 (contarLineaDif turno (cdr linea))))))))


; 4.2.1. Conjunto de candidatos: Obtiene las posiciones disponibles en la matriz
(define (candidatos mat)
  (candidatosAux (length mat) (length (car mat)) 1 1 '() mat))

(define (candidatosAux filas cols i j posLista mat)
  (cond
    ((equal? i (+ filas 1)) posLista)
    ((equal? j (+ cols 1)) (candidatosAux filas cols (+ i 1) 1 posLista mat))
    ((equal? '- (getEnMat i j mat)) (candidatosAux filas cols i (+ j 1) (append posLista (list (list i j))) mat))
    (else (candidatosAux filas cols i (+ j 1) posLista mat))))

;Funcion que devuelve una lista con listas con los indices de todas las posiciones disponibles en una linea
(define (getPosVacias linea mat)
  (getPosVaciasAux 1 (getEn 1 linea) (getEn 2 linea) (getEn 4 linea) (length (getEn 4 linea)) (length mat) (length (car mat))'() ))
(define (getPosVaciasAux aux1 tipo num linea lenLinea filas cols res)
  (cond
    ((zero? lenLinea) res)
    ((equal? tipo 'row)
     (cond
       ((equal? (getEn lenLinea linea) '-) (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) filas cols (append (list (list num lenLinea)) res)))
       (else (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) filas cols res))))
    ((equal? tipo 'column)
     (cond
       ((equal? (getEn lenLinea linea) '-) (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) filas cols (append (list (list lenLinea num )) res)))
       (else (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) filas cols res))))
    ((equal? tipo 'diagonal)
     (cond
       ((and (equal? (getEn lenLinea linea) '-) (> filas cols));REVISAAAAAAAAAAAAAAAR
        (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) (- filas 1) (- cols 1) (append (list (list (+ cols (- num 1)) lenLinea)) res)))
       ((and (equal? (getEn lenLinea linea) '-) (> cols filas))
        (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) (- filas 1) (- cols 1) (append (list (list filas (+ filas (- num 1)))) res)))
       ((and (equal? (getEn lenLinea linea) '-) (equal? filas cols))
        (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) (- filas 1) (- cols 1) (append (list (list lenLinea lenLinea)) res)))
       (else (getPosVaciasAux aux1 tipo num linea (- lenLinea 1) (- filas 1) (- cols 1) res))))
    ((equal? tipo 'invdiagonal)
     (cond
       ((and (equal? (getEn lenLinea linea) '-) (> filas cols))
        (getPosVaciasAux (+ aux1 1)  tipo  num linea (- lenLinea 1) (- filas 1) (- cols 1) (append (list (list (+ lenLinea (- num 1)) aux1)) res)))
       ((and (equal? (getEn lenLinea linea) '-) (equal? filas cols))
        (getPosVaciasAux (+ aux1 1) tipo num linea (- lenLinea 1) (- filas 1) (- cols 1) (append (list (list lenLinea aux1)) res)))         
       ((and (equal? (getEn lenLinea linea) '-) (> cols filas))
        (getPosVaciasAux (+ aux1 1) tipo (+ num 1) linea (- lenLinea 1) (- filas 1) (- cols 1) (append (list (list filas num)) res)))
       (else
        (cond
          ((or (> filas cols) (equal? filas cols)) (getPosVaciasAux (+ aux1 1) tipo num linea (- lenLinea 1) (- filas 1) (- cols 1) res))
          (else (getPosVaciasAux (+ aux1 1) tipo (+ num 1) linea (- lenLinea 1) (- filas 1) (- cols 1) res))))))))
          


; 4.2.2. Función de selección: Obtiene la línea más prometedora para ganar
(define (seleccion turno mat)
  (seleccionAux turno mat 11 '()))

(define (seleccionAux turno mat mayor res)
  (cond
    ((< (getEn 3 (getFilaOptima  turno  1 11 (length mat) mat '())) mayor)
     (seleccionAux turno mat (getEn 3 (getFilaOptima  turno  1 11 (length mat) mat '())) (getFilaOptima  turno  1 11 (length mat) mat '())))
    ((< (getEn 3 (getColOptima turno 1 11 (length (car mat)) mat '())) mayor)
     (seleccionAux turno mat (getEn 3 (getColOptima turno 1 11 (length (car mat)) mat '())) (getColOptima turno 1 11 (length (car mat)) mat '())))
    ((< (getEn 3 (getDiagOptima turno 1 11 (length (getDiags mat)) (getDiags mat) '()))  mayor)
     (seleccionAux turno mat (getEn 3 (getDiagOptima turno 1 11 (length (getDiags mat)) (getDiags mat) '())) (getDiagOptima turno 1 11 (length (getDiags mat)) (getDiags mat) '()))) 
    ((< (getEn 3 (getDiagInvOptima turno 1 11 (length (getDiagsInv mat)) (getDiagsInv mat) '())) mayor)
     (seleccionAux turno mat (getEn 3 (getDiagInvOptima turno 1 11 (length (getDiagsInv mat)) (getDiagsInv mat) '())) (getDiagInvOptima turno 1 11 (length (getDiagsInv mat)) (getDiagsInv mat) '())))
    (else
     (cond
       ((null? res) res)
       (else (append res (list (getPosVacias res mat))))))))
  


;Funcion que devuelve el tipo, numero de fila, cantidad de tokens y fila donde hay mas chance de ganar
(define (getFilaOptima turno mejorIndice mayor filas mat res)
  (cond
    ((zero? filas) (append (list 'row) (list mejorIndice  mayor) (list res))) 
    ((and (< (objetivo turno (getEn filas mat)) mayor) (zero? (contarLineaDif turno (getEn filas mat))))
     (getFilaOptima  turno filas (objetivo turno (getEn filas mat)) (- filas 1) mat (getEn filas mat)))
    (else (getFilaOptima  turno mejorIndice  mayor (- filas 1) mat res))))


;Funcion que devuelve el tipo, numero de columna, cantidad de tokens y columna donde hay mas chance de ganar
(define (getColOptima turno mejorIndice mayor cols mat res)
  (cond
    ((zero? cols) (append (list 'column) (list mejorIndice mayor) (list res))) 
    ((and (< (objetivo turno (getCol cols mat)) mayor) (zero? (contarLineaDif turno (getCol cols mat))))
     (getColOptima turno cols (objetivo turno (getCol cols mat)) (- cols 1) mat (getCol cols mat)))
    (else (getColOptima turno mejorIndice mayor (- cols 1) mat res))))

;Funcion que devuelve el tipo, numero de diagonal (de izquierda a derecha, arriba a abajo), cantidad de tokens y diagonal donde hay mas chance de ganar
(define (getDiagOptima turno mejorIndice mayor numDiags diags res)
  (cond
    ((not (list? (car diags)))
     (cond
       ((zero? (contarLineaDif turno diags)) (append (list 'diagonal) (list 1 (objetivo turno diags)  diags)))
       (else (append (list 'diagonal) (list 1 11 '())))))
    ((zero? numDiags) (append (list 'diagonal) (list mejorIndice mayor) (list res)))
    ((and (< (objetivo turno (getEn numDiags diags)) mayor) (zero? (contarLineaDif turno(getEn numDiags diags))))
     (getDiagOptima turno numDiags (objetivo turno (getEn numDiags diags)) (- numDiags 1) diags (getEn numDiags diags)))
    (else (getDiagOptima turno mejorIndice mayor (- numDiags 1) diags res))))

;Funcion que devuelve el tipo, numero de diagonal inversa (de izquierda a derecha, abajo a arriba), cantidad de tokens y diagonal inversa donde hay mas chance de ganar
(define (getDiagInvOptima turno mejorIndice mayor numDiagsInv invDiags res)
  (cond
    ((not (list? (car invDiags)))
     (cond
       ((zero? (contarLineaDif turno invDiags)) (append (list 'invdiagonal) (list 1 (objetivo turno invDiags) invDiags)))
       (else (append (list 'invdiagonal) (list 1 11 '())))))
    ((zero? numDiagsInv) (append (list 'invdiagonal) (list mejorIndice mayor) (list res)))
    ((and (< (objetivo turno (getEn numDiagsInv invDiags)) mayor) (zero? (contarLineaDif turno(getEn numDiagsInv invDiags))))
     (getDiagInvOptima turno numDiagsInv (objetivo turno (getEn numDiagsInv invDiags)) (- numDiagsInv 1) invDiags (getEn numDiagsInv invDiags)))
    (else (getDiagInvOptima turno mejorIndice mayor (- numDiagsInv 1) invDiags res))))

; 4.2.3. Función de viabilidad: Verifica la viabilidad de colocar un token en una posición
(define (viabilidad jug1 jug2 mat)
  (viabilidadAux jug1 jug2 (seleccion jug1 mat) (seleccion jug2 mat) (candidatos mat) mat))

(define (viabilidadAux jug1 jug2 jugada1 jugada2 posDisp mat)
  (cond
    ((and (null? jugada1) (null? jugada2)) "gameover")
    ((and (not (null? jugada2)) (equal? (length (getEn 5 jugada2)) 1)) (getEn 1 (getEn 5 jugada2)))
    ((null? jugada1) (getEn 1 (getEn 5 jugada2))) 
    ((equal? (length (getEn 5 jugada1)) 1) (getEn 1 (getEn 5 jugada1)))
    (else
     (cond
       ((null? jugada2) (getEn 1 (getEn 5 jugada1)))
       ((not (null? (interseccion (getEn 5 jugada1) (getEn 5 jugada2)))) (car (interseccion (getEn 5 jugada1) (getEn 5 jugada2))))
       (else (getEn 1 (getEn 5 jugada1)))))))

;Funcion que coloca el token en la posicion mas viable

(define (ponerToken jug1 jug2 mat)
  (cond
    ((list? (viabilidad jug1 jug2 mat))
     (print (viabilidad jug1 jug2 mat))
     (newline)
     (setEnMat (car (viabilidad jug1 jug2 mat)) (cadr (viabilidad jug1 jug2 mat)) jug2 mat))
    (else (append (list "No movements left to win") mat))))


;Funcion que declara si un elemento esta en una lista o no
(define (miembro ele lista)
  (cond
    ((null? lista) #f)
    ((equal? (car lista) ele) #t)
    (else (miembro ele (cdr lista)))))

;Funcion que devuelve los elementos de la interseccion entre dos listas
(define (interseccion linea1 linea2)
  (cond
    ((null? linea1) '())
    ((miembro (car linea1) linea2) (append (list (car linea1)) (interseccion (cdr linea1) linea2)))
    (else (interseccion (cdr linea1) linea2))))

; 4.2.4. Función objetivo: Cuenta cuántos elementos faltan para llenar la línea
(define (objetivo turno fila)
  (objetivoAux turno fila (length fila) 0))

(define (objetivoAux turno fila lenFila res)
  (cond
    ((null? fila) (- lenFila res))
    ((equal? (car fila) turno) (objetivoAux turno (cdr fila) lenFila (+ res 1)))
    (else (objetivoAux turno (cdr fila) lenFila res))))

; 4.2.5. Función de solución: Verifica si hay una victoria
(define (solucion turno mat)
  (cond
    ((or (solucionFila turno mat)
         (solucionCol turno mat)
         (solucionDiag turno mat)) #t)
    (else #f)))

;#############################################################################################################################

(define (juego jugador1 jugador2 mat turno)
  (cond
    ((equal? (car mat) "No quedan movimientos"))
    ((solucion jugador1 mat) )
    ((solucion jugador2 mat) )
    (else
     (cond
       ((zero? turno)
        (define entry (jugTurno))
        (cond
          ((miembro entry (candidatos mat))
           (juego jugador1 jugador2 (setEnMat (car entry) (cadr entry) jugador1 mat) 1))
          (else
           (print "Posicion ocupada")
           (juego jugador1 jugador2 mat turno) )))
       
       (else (juego jugador1 jugador2 (ponerToken jugador1 jugador2 mat) 0))))))


;Funcion que obtiene la entrada del usuario

(define (jugTurno)
  (display "Fila: ")
  (define row (read-line))
  (display "Columna: ")
  (define col (read-line))
  (append (list (string->number row)) (list (string->number col))))