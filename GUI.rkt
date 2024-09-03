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

(define (llamadaBoton button event)
  (let ((numColumnas (send entradaColumnas get-value))
        (numFilas (send entradaFilas get-value)))
    
    (cond
      [(> (string->number numColumnas) 10)
       (new message%
            [parent box5]
            [label "No se puede crear un tablero de más de 10 columnas"]
            [font (make-object font% 14.0 'system)])]
      
      [(> (string->number numFilas) 10)
       (new message%
            [parent box5]
            [label "No se puede crear un tablero de más de 10 filas"]
            [font (make-object font% 14.0 'system)])])))
      

;"""""""""""""""""""""""""""""""""""Botón para validar datos ingresados""""""""""""""""""""""
;Crea el botón check    
(new button% [label "Validar"]
    [parent box4]
    [vert-margin 50]
    [min-width 100]
    [font (make-object font% 20.0 'system)]
    [callback llamadaBoton])
    

; Centrar la ventana
(send Ventana1 center)

; Mostrar la ventana
(send Ventana1 show #t)