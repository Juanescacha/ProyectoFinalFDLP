#lang eopl

;(require racket/string)    ; Importamos libereria para el manejo de strings

; Integrantes:
; Juan Esteban Camargo Chacon - 1924984
;
;<programa>  := <expresion> {<expresion>}* 
;                un-programa (exp)
;
;
;<expresion> := <numero>
;               <numero (num)>
;
;            := :<texto>:
;               <texto (txt)>
;
;            := <identificador>
;               <identificador (id)>
;
;            := true
;               <true-value>
;
;            := false
;               <false-value>
;
;            := [x32 numero {<numero>}* ]
;               <base-32-numero>
;
;            := [x16 numero {<numero>}* ]
;               <base-16-numero>
;
;            := [x8 numero {<numero>}* ]
;               <base-8-numero>
;
;            := MEZCLAVAL
;               <valor-MEZCLAVAL>
;
;            := (lista <expresion> , {<expresion>}*(,) )
;
;            CARACTERISTICAS EXPRESIONES
;
;            := var <identificador> = <expresion>;
;               <set-expresion (id exp)>
;
;            := const <identificador> = <expresion>;
;               <set-const-expresion (id exp);
;
;            := letrec ( {<identificador> = <expresion>}*(;) ) in { <expresion> }
;               <letrec-expresion (ids exps cuerpo)>
;
;            := function ( {<indentificador>}*(,) ) { <expresion> }
;               <procedimiento-expresion (ids cuerpo)>
;
;            := function_rec ( {<indentificador>}*(,) ) { <expresion> }
;               <procedimiento-expresion (ids cuerpo)>
;
;            := <expresion> ( {<expresion>}* )
;               <evaluar-expresion exp exps>
;
;            := ( expresion <primitivas-binarias> expresion )
;               <prim-bin-expresion (exp1 prim-bin exp2)>
;
;            := { <primitivas-unarias> expresion }
;               <prim-un-expresion (prim-un exp)>
;
;            := if ( <expresion> ) { <expresion> } else { <expresion> }
;               <if-expresion (test-exp true-exp false-exp)>
;
;<primitivas-binarias> := <primitivas-binarias-10>
;
;                      := <primitivas-binarias-8>
;
;                      := <primitivas-binarias-16>
;
;                      := <primitivas-binarias-32>
;
;                      := <primitivas-binarias-booleanas>
;
;                      := <primitivas-binarias-listas>
;
;
;<primitivas-unarias> := add1 (primitiva-add1)
;
;                     := sub1 (primitiva-sub1)
;
;                     := <primitivas-unarias-booleanas>
;
;                     := <primitivas-unarias-listas>
;
;
;<primitivas-binarias-10> :=  + (primitiva-suma)
;
;                         :=  - (primitiva-resta)
;
;                         :=  * (primitiva-multiplicacion)
;
;                         :=  / (primitiva-division)
;
;                         :=  % (primitiva-modulo)
;
;
;<primitivas-binarias-8> :=  +8 (primitiva-suma-8)
;
;                        :=  -8 (primitiva-resta-8)
;
;                        :=  *8 (primitiva-multiplicacion-8)
;
;
;<primitivas-binarias-16> :=  +16 (primitiva-suma-16)
;
;                         :=  -16 (primitiva-resta-16)
;
;                         :=  *16 (primitiva-multiplicacion-16)
;
;
;<primitivas-binarias-32> :=  +32 (primitiva-suma-32)
;
;                         :=  -32 (primitiva-resta-32)
;
;                         :=  *32 (primitiva-multiplicacion-32)
;
;
;<primitivas-binarias-booleanas> := concatenar (primitiva-concatenar)
;
;                                := cons (primitiva-cons)
;
;                                := < (primitiva-menor)
;
;                                := > (primitiva-mayor)
;
;                                := menor= (primitiva-menor-igual)
;
;                                := mayor= (primitiva-mayor-igual)
;
;                                := == (primitiva-igual)
;
;                                := != (primitiva-diferente)
;
;                                := && (primitiva-and)
;
;                                := ^^ (primitiva-or)
;
;
;<primitivas-unarias-booleanas> := not (primitiva-not)
;
;
;<primitivas-unarias-listas> := empty?
;
;                            := length (primitiva-longitud)
;
;                            := list? (primitiva-booleana-lista)
;
;                            := car (primitiva-car)
; 
;                            := cdr (primitiva-cdr)
;
;<primitivas-binarias-listas> := append (primitiva-append)
;
;
;***********************************************************************************


; DEFINICION LEXICA Y GRAMATICA

(define lexica
  '(
    (espacios (whitespace) skip)
    (cometarios ("%" (arbno (not #\newline))) skip)
    (identificador ("$" letter (arbno (or letter digit "?"))) symbol)
    
    ;enteros
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    
    ;flotantes
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)

    ;texto
    (text (letter (arbno (or letter digit whitespace "?" "=" "'" "," "." ))) string)
    )
  )


(define gramatica
  '(
    ; programa
    (programa (expresion) un-programa)
    
    ; expresion
    (expresion (numero) numero-lit)
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

;***********************************************************************************