#lang eopl

; DEFINICION LEXICA Y GRAMATICA

(define lexica
  '(
    (espacios (whitespace) skip)
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)
    
    ; enteros
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    
    ; decimales
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

(define gramatica
  '(
    ; programa
    (programa (expresion) un-programa)

    ;expresion
    (expresion (numero) nada)
    )
  )

;***********************************************************************************

; USO LIBRERIA SLLGEN

; Definicion Construccion automatica de los Datatypes con SLLGEN

(sllgen:make-define-datatypes lexica gramatica)

(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes lexica gramatica)))

; Parser

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

; Scanner

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

; Crear Interpretador

(define interpretador
  (sllgen:make-rep-loop "C:/"
                        (lambda (pgm) pgm)
                        (sllgen:make-stream-parser lexica gramatica)))
