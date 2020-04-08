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
;            := true
;               <true-expresion>
;
;            := false
;               <false-expresion>
;
;
;<primitivas-booleana-binaria> := < (primitiva-booleana-menor)
;
;                              := > (primitiva-booleana-mayor)
;
;                              := menor= (primitiva-booleana-menor-igual)
;
;                              := mayor= (primitiva-booleana-mayor-igual)
;
;                              := == (primitiva-booleana-igual)
;
;                              := != (primitiva-booleana-diferente)
;
;                              := && (primitiva-booleana-and)
;
;                              := ^^ (primitiva-booleana-or)
;
;                              := not (primitiva-booleana-not) (unaria)
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
;
;<primitivas-unarias> := add1 (primitiva-add1)
;
;                     := sub1 (primitiva-subb1)
;
;                     := longitud (primitiva-longitud)
;
;
;<primitivas-binarias-base>     :=   +  (primitiva-base-suma)
;
;                               :=   -  (primitiva-base-resta)
;
;                               :=   *  (primitiva-base-multiplicacion)
;
;
;<primitiva-lista> := empty?
;                  := list?
;                  := car
;                  := cdr
;
;                  := cons
;                  := append
;
;
;<numeros-bases> := [x32 numero {<numero>}* ]
;                   <base-32>
;
;                := [x16 numero {<numero>}* ]
;                   <base-16>
;
;                := [x8 numero {<numero>}* ]
;                   <base-8>
;
;
;***********************************************************************************