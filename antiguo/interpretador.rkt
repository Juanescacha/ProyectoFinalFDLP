#lang eopl

(require racket/string)    ; Importamos libereria para el manejo de strings

; Integrantes:
; Juan Esteban Camargo Chacon - 1924984
; Brayan Rincones - 1673847
; Sebastian Saldaña - 1623447

;<programa> :=  <expresion>
;               un-programa (exp)
;
;
;<expresion> := <numero>
;               <numero-expresion (num)>
;
;            := :<texto>:
;               <texto-expresion (txt)>
;
;            := !<identificador>
;               <var-expresion (id)>
;
;            := <primitivas-binarias> ( expresion expresion )
;               <prim-bin-expresion (prim-bin exp1 exp2)>
;
;            := <primitivas-unarias> ( expresion )
;               <prim-un-expresion (prim-un exp)>
;
;            := if ( <expresion> ) then { <expresion> } else { <expresion> }
;               <if-expresion (test-exp true-exp false-exp)>
;
;            := let ( {<indentificador> = <expresion>}*(;) ) in { <expresion> }
;               <let-expresion (ids exps cuerpo)>
;
;            := letrec ( {<identificador> = <expresion>}*(;) ) in { <expresion> }
;               <letrec-expresion (ids exps cuerpo)>
;
;            := constantes ( {<indentificador> = <expresion>}*(;) ) in { <expresion> }
;               <constantes-expresion (ids exps cuerpo)>
;
;            := procedimiento ( {<indentificador>}*(,) ) haga { <expresion> }
;               <procedimiento-expresion (ids cuerpo)>
;
;            := ( <expresion> {<expresion>}* )
;               <evaluar-expresion exp exps>
;
;            := true
;               <true-expresion>
;
;            := false
;               <false-expresion>
;
;            := [x32 numero {<numero>}* ]
;               <base-32>
;
;            := [x16 numero {<numero>}* ]
;               <base-16>
;
;            := [x8 numero {<numero>}* ]
;               <base-8>
;
;            := MEZCLAVAL
;               <valor-null-expresion>
;
;
;<primitivas-binarias> :=  + (primitiva-suma)
;
;                      :=  - (primitiva-resta)
;
;                      :=  * (primitiva-multiplicacion)
;
;                      :=  / (primitiva-division)
;
;                      :=  % (primitiva-modulo)
;
;                      := concatenar (primitiva-concatenar)
;
;                      := cons (primitiva-cons)
;
;                      := append (primitiva-append)
;
;                      := < (primitiva-booleana-menor)
;
;                      := > (primitiva-booleana-mayor)
;
;                      := menor= (primitiva-booleana-menor-igual)
;
;                      := mayor= (primitiva-booleana-mayor-igual)
;
;                      := == (primitiva-booleana-igual)
;
;                      := != (primitiva-booleana-diferente)
;
;                      := && (primitiva-booleana-and)
;
;                      := ^^ (primitiva-booleana-or)
;
;
;<primitivas-unarias> := add1 (primitiva-add1)
;
;                     := sub1 (primitiva-sub1)
;
;                     := longitud (primitiva-longitud)
;
;                     := not (primitiva-booleana-not)
;
;                     := empty? (primitiva-booleana-empty)
;
;                     := list? (primitiva-booleana-list)
;
;                     := car (primitiva-car)
;
;                     := cdr (primitiva-cdr)
;
;
;***********************************************************************************


; DEFINICION LEXICA Y GRAMATICA

(define lexica
  '(
    (espacios (whitespace) skip)
    (cometarios ("%" (arbno (not #\newline))) skip)
    (identificador ("!" letter (arbno (or letter digit "?"))) symbol)
    
    ; enteros
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    
    ; flotantes
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)

    ; texto
    (text (":" (or letter whitespace) (arbno (or letter digit whitespace "?" "=" "'" "," "." )) ":") string)
    )
  )


(define gramatica
  '(
    ; programa
    (programa (expresion) un-programa)
    
    ; expresion
    (expresion (numero) numero-expresion)
    (expresion (text) texto-expresion)
    (expresion (identificador) var-expresion)
    (expresion (primitivas-binarias "(" expresion expresion ")") prim-bin-expresion)
    (expresion (primitivas-unarias "(" expresion ")") prim-un-expresion)
    (expresion ("if" "(" expresion ")" "then" "{" expresion "}" "else" "{" expresion "}") if-expresion)
    (expresion ("let" "(" (separated-list identificador "=" expresion ";") ")" "in" "{" expresion "}") let-expresion)
    (expresion ("letrec" "(" (separated-list identificador "=" expresion ";") ")" "in" "{" expresion "}") letrec-expresion)
    (expresion ("constantes" "(" (separated-list identificador "=" expresion ";") ")" "in" "{" expresion "}") constantes-expresion)
    (expresion ("procedimiento" "(" (separated-list identificador ",") "haga" "{" expresion "}") procedimiento-expresion)
    (expresion ("(" expresion (arbno expresion) ")") evaluar-expresion)
    (expresion ("true") true-expresion)
    (expresion ("false") false-expresion)
    (expresion ("[x32" numero (arbno numero) "]") base-32)
    (expresion ("[x16" numero (arbno numero) "]") base-16)
    (expresion ("[x8" numero (arbno numero) "]") base-8)
    (expresion ("MEZCLAVAL") valor-null-expresion)

    ;primitivas-binarias
    (primitivas-binarias ("+") primitiva-suma)
    (primitivas-binarias ("-") primitiva-resta)
    (primitivas-binarias ("*") primitiva-multiplicacion)
    (primitivas-binarias ("/") primitiva-division)
    (primitivas-binarias ("%") primitiva-modulo)
    (primitivas-binarias ("concatenar") primitiva-concatenar)
    (primitivas-binarias ("cons") primitiva-cons)
    (primitivas-binarias ("append") primitiva-append)
    (primitivas-binarias ("<") primitiva-booleana-menor)
    (primitivas-binarias (">") primitiva-booleana-mayor)
    (primitivas-binarias ("menor=") primitiva-booleana-menor-igual)
    (primitivas-binarias ("mayor=") primitiva-booleana-mayor-igual)
    (primitivas-binarias ("==") primitiva-booleana-igual)
    (primitivas-binarias ("!=") primitiva-booleana-diferente)
    (primitivas-binarias ("&&") primitiva-booleana-and)
    (primitivas-binarias ("^^") primitiva-booleana-or)

    ;primitivas-unarias
    (primitivas-unarias ("add1") primitiva-add1)
    (primitivas-unarias ("sub1") primitiva-sub1)
    (primitivas-unarias ("longitud") primitiva-longitud)
    (primitivas-unarias ("car") primitiva-car)
    (primitivas-unarias ("cdr") primitiva-cdr)
    (primitivas-unarias ("not") primitiva-booleana-not)
    (primitivas-unarias ("empty?") primitiva-booleana-empty)
    (primitivas-unarias ("list?") primitiva-booleana-list)
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