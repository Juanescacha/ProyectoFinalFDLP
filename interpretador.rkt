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
;            := expresion <primitiva-binaria> expresion
;               <primapp-bin-exp (exp1 prim-binaria exp2)>
;
;            := <primitiva-unaria> expresion
;               <primapp-un-exp (prim-unaria exp)>
;
;            := if <expresion> then <expresion> else <expresion> endIf
;               <if-expresion (test-exp true-exp false-exp)>
;
;            := let ( {<indentificador> = <expresion>}*(;) ) {<expresion>}
;               <let-expresion (ids exps cuerpo)>
;
;            := letrec {<identificador> ({<indetificador>}*(,)) = <expresion>}* in <expresion>
;               <letrec-expresion ( proc-names ids bodies letrec-body )>
;
;            := procedimiento ( {<indentificador>}*(,)) haga <expresion> finProc
;               <procedimiento-expresion (ids cuerpo)>
;
;            := (<expresion> {<expresion>}*( ) )
;               <evaluar-expresion exp exps>
;
;
;<primitiva-booleana> := < (primitiva-booleana-menor)
;
;                     := > (primitiva-booleana-mayor)
;
;                     := menor= (primitiva-booleana-menor-igual)
;
;                     := mayor= (primitiva-booleana-mayor-igual)
;
;                     := == (primitiva-booleana-igual)
;
;                     := != (primitiva-booleana-diferente)
;
;                     := && (primitiva-booleana-and)
;
;                     := ^^ (primitiva-booleana-or)
;
;                     := not (primitiva-booleana-not) (unaria)
;
;
;<primitiva-aritmetica-enteros-b> :=  + (primitiva-suma)
;
;                                 :=  - (primitiva-resta)
;
;                                 :=  * (primitiva-multiplicacion)
;
;                                 :=  / (primitiva-division)
;
;                                 :=  % (primitiva-modulo)
;
;
;<primitiva-aritmetica-enteros-u> := add1 (primitiva-add1)
;
;                                 := sub1 (primitiva-subb1)
;
;
;
;
;<primitiva-aritmetica-base-b> :=   +  (primitiva-base-suma)
;
;                              :=   -  (primitiva-base-resta)
;
;                              :=   *  (primitiva-base-multiplicacion)
;
;
;<primitiva-aritmetica-base-u> := add1 (primitiva-base-add1)
;
;                              := sub1 (primitiva-base-subb1)
;
;
;
;
;<primitiva-cadena-b> := concatenar (primitiva-concatenar)
;
;<primitiva-cadena-u> := longitud   (primitiva-longitud)
;
;
;<primitiva-lista> := empty?
;                  := empty
;                  := cons
;                  := list?
;                  := car
;                  := cdr
;                  := append
;
;
;<numeros-otras-bases> := x32
;                         <base-32>
;
;                      := x16
;                         <base-16>
;
;                      := x8
;                         <base-8>
;
;
;***********************************************************************************