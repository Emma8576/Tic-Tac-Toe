;"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;                                                 Tarea #2                                           ;
;                                        Paradigmas de programación                                  ;
;                                 Desarrollo de Juego Tic Tac Toe en Racket                          ;
;                                               Programadores:                                       ;
;                                               Asly Barahona                                        ;
;                                              Joaquín Ramirez                                       ;
;                                              Fabián Gutierrez                                      ;
;                                               Emmanuel Calvo                                       ;
;"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#lang racket/gui
(require racket/gui/base)
(require "Logica.rkt")


; Crear la ventana de inicio y bienvenida
(define Ventana1
  (new frame%
       [label "Inicio"]
       [width 700]
       [height 700]))

;"""""""""""""""""""""""""Almacenar mensajes en pantalla""""""""""""""""""""""""""""""

; Crear una caja para guardar la etiqueta de bienvenida
(define box1
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para guardar la etiqueta de tamaño de matriz
(define box2
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para las entradas de texto
(define box3
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para las entradas de texto
(define box4
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para mostrar mensaje de error de entrada
(define box5
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

;"""""""""""""""""""""""""Mostrar mensajes en pantalla""""""""""""""""""""""""""""""

; Mostrar mensaje de Bienvenida
(define msg-area (new message%
                      [parent box1]
                      [vert-margin 100]
                      [label "  Bienvenido a Tic Tac Toe"]
                      [min-width 250]
                      [font (make-object font% 25.0 'system)]
                      [auto-resize #t]))

; Mostrar mensaje de elegir tamaño de matriz
(define msg-area2 (new message%
                      [parent box2]
                      [vert-margin 30]
                      [label "  Por favor ingrese el tamaño del tablero"]
                      [min-width 250]
                      [font (make-object font% 18.0 'system)]
                      [auto-resize #t]))

;""""""""""""""""""""""" Crear las cajas de entrada de texto"""""""""""""""""""""""""""""""

(define entradaColumnas (new text-field%
                       [label "Columnas"]
                       [parent box3]
                       [min-width 100]  ; Ancho fijo para la caja de texto
                       [min-height 30]  ; Altura fija para la caja de texto
                       [font (make-object font% 14.0 'system)])) ; Tamaño del texto

(define entradaFilas (new text-field%
                       [label "     Filas"]
                       [parent box3]
                       [min-width 100]  ; Ancho fijo para la caja de texto
                       [min-height 30]  ; Altura fija para la caja de texto
                       [font (make-object font% 14.0 'system)])) ; Tamaño del texto

; Definir variables globales para los mensajes de error
(define mensaje-error-columnas #f)
(define mensaje-error-filas #f)

; Crear dos cajas separadas para los mensajes de error
(define box-error-columnas
  (new vertical-panel%
       [parent Ventana1]
       [alignment '(center top)]
       [stretchable-height #f]))

(define box-error-filas
  (new vertical-panel%
       [parent Ventana1]
       [alignment '(center top)]
       [stretchable-height #f]))

; Función para actualizar o crear un mensaje de error
(define (actualizar-mensaje-error box mensaje texto)
  (if mensaje
      (begin
        (send mensaje set-label texto)
        (send mensaje show #t))
      (set! mensaje (new message% 
                         [parent box] 
                         [label texto] 
                         [font (make-object font% 14.0 'system)])))
  mensaje)

; Función que se llama cuando se presiona el botón
(define (llamadaBoton button event)
  (let ((numColumnas (string->number (send entradaColumnas get-value)))
        (numFilas (string->number (send entradaFilas get-value))))
    
    ; Actualizar mensaje de error para columnas
    (set! mensaje-error-columnas
      (cond
        [(or (not numColumnas) (< numColumnas 3))
         (actualizar-mensaje-error box-error-columnas mensaje-error-columnas 
                                   "No se puede crear un tablero de menos de 3 columnas")]
        [(> numColumnas 10)
         (actualizar-mensaje-error box-error-columnas mensaje-error-columnas 
                                   "No se puede crear un tablero de más de 10 columnas")]
        [else 
         (when mensaje-error-columnas
           (send mensaje-error-columnas show #f))
         #f]))
    
    ; Actualizar mensaje de error para filas
    (set! mensaje-error-filas
      (cond
        [(or (not numFilas) (< numFilas 3))
         (actualizar-mensaje-error box-error-filas mensaje-error-filas 
                                   "No se puede crear un tablero de menos de 3 filas")]
        [(> numFilas 10)
         (actualizar-mensaje-error box-error-filas mensaje-error-filas 
                                   "No se puede crear un tablero de más de 10 filas")]
        [else 
         (when mensaje-error-filas
           (send mensaje-error-filas show #f))
         #f]))
    
    ; Si no hay errores, llamar a Cuadricula
    (when (and (not mensaje-error-columnas) (not mensaje-error-filas))
      (displayln "Llamando a Cuadricula")
      (Cuadricula numFilas numColumnas 1 1))))

;"""""""""""""""""""""""""""""""""""Botón para validar datos ingresados""""""""""""""""""""""
;Crea el botón check    
(new button% [label "Validar"]
    [parent box4]
    [vert-margin 50]
    [min-width 100]
    [font (make-object font% 20.0 'system)]
    [callback llamadaBoton])
    
; Centrar la ventana1
(send Ventana1 center)

; Mostrar la ventana
(send Ventana1 show #t)

;""""""""""""""""""""""""""""""""""""""""""""""Fin Ventana1"""""""""""""""""""""""""""""""""""""""

;""""""""""""""""""""""""""""""""""""""""""""""Inicio Ventana2"""""""""""""""""""""""""""""""""""""""
(define Ventana2 (new frame%
                      [label "Tic Tac Toe"]
                      [width 700]
                      [height 700]))

; Lista de listas para almacenar los botones
(define botones-lista '())

; Variables globales
(define Matrix '())
(define Enable #t)

; Función para crear la cuadrícula
(define (Cuadricula Fila Columna auxFila auxColumna)
  ; Definir el tamaño de cada botón
  (define button-size 60)

  ; Calcular el tamaño de la ventana en función del número de filas y columnas
  (define window-width (* Columna button-size))
  (define window-height (* Fila button-size))

  ; Ajustar el tamaño de la ventana 2
  (send Ventana2 resize window-width window-height)
  (send Ventana2 show #t)  ; Mostrar la ventana 2
  (send Ventana2 center)   ; Centrar la ventana
  (send Ventana1 show #f)  ;Ocultar ventana 1

  ; Crear un panel para la cuadrícula
  (define grid-panel
    (new vertical-panel%
         [parent Ventana2]
         [alignment '(center top)]))  ; Panel que contiene filas y columnas

  ; Crear la cuadrícula de botones
  (for ([i (in-range Fila)])  ; Crear las filas
    (define row-panel (new horizontal-panel%
                           [parent grid-panel]))
    (define fila-botones '())  ; Lista para almacenar los botones de esta fila
    (for ([j (in-range Columna)])  ; Crear las columnas
      (define btn (new button%
                       [label ""]
                       [parent row-panel]
                       [min-width button-size]  ; Tamaño de cada botón
                       [min-height button-size]
                       [font (make-object font% 25.0 'system)]
                       [callback (lambda (button event) (click-handler button event (add1 i) (add1 j)))]))  ; Asignar el manejador de clics a cada botón
      (set! fila-botones (append fila-botones (list btn))))  ; Añadir el botón a la lista de la fila
    (set! botones-lista (append botones-lista (list fila-botones))))  ; Añadir la fila a la lista principal

  ; Imprimir la estructura de la lista de botones (para depuración) Quitar luego
  (for ([fila botones-lista]
        [i (in-naturals 1)])
    (printf "Fila ~a: ~a botones\n" i (length fila)))

  ; Inicializar la matriz del juego
  (set! Matrix (genMat Fila Columna)))

; Función para manejar los clics en cada casilla
(define (click-handler btn event i j)
  (when Enable
    (cond
      ((equal? (getEnMat i j Matrix) '-)
       (send btn set-label "X")
       (set! Matrix (setEnMat i j 'O Matrix))
       (cond
         ((solucion 'O Matrix)
          (display-victory "Player wins"))
         ((equal? (car Matrix) "No movements left to win")
          (display-tie))
         (else
          (set! Enable #f)
          (sleep/yield 0.5)
          (machine-turn))))
      (else (display "Posición ocupada")))))

(define (machine-turn)
  (set! Matrix (ponerToken 'O 'X Matrix))
  (update-gui-after-machine-move)
  (cond
    ((solucion 'X Matrix)
     (display-defeat "Computer wins"))
    ((equal? (car Matrix) "No movements left to win")
     (display-tie))
    (else
     (set! Enable #t))))

(define (update-gui-after-machine-move)
  (for ([i (in-range (length Matrix))]
        [row-buttons botones-lista])
    (for ([j (in-range (length (car Matrix)))]
          [btn row-buttons])
      (when (equal? (getEnMat (add1 i) (add1 j) Matrix) 'X)
        (send btn set-label "O")))))

; Funciones para mostrar el resultado del juego (debes implementarlas)
(define (display-victory message)
  (displayln message))

(define (display-defeat message)
  (displayln message))

(define (display-tie)
  (displayln "It's a tie!"))

; Iniciar el juego
(send Ventana1 show #t)