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
;               <letrec-expresion ( proc-names ids bodies letrec-body )>
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
;                     := sub1 (primitiva-subb1)
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