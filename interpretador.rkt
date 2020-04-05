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
;               numero-lit (num)
;
;            := :<texto>:
;               <texto-lit (txt)>
;
;            := !<identificador>
;               <var-exp (id)>
;
;            := (expresion <primitiva-binaria> expresion)
;               <primapp-bin-exp (exp1 prim-binaria exp2)>
;
;            := <primitiva-unaria> (expresion)
;               <primapp-un-exp (prim-unaria exp)>
;
;            := Si <expresion> entonces <expresion> sino <expresion> finSI
;               <condicional-exp (test-exp true-exp false-exp)>
;
;            := declarar ( {<indentificador> = <expresion>}*(;) ) {<expresion>}
;               <variableLocal-exp (ids exps cuerpo)>
;
;            := letrec {<identificador> ({<indetificador>}*(,)) = <expresion>}* in <expresion>
;               <letrec-exp ( proc-names ids bodies letrec-body )>
;
;            := procedimiento ( {<indentificador>}*(,)) haga <expresion> finProc
;               <procedimiento-exp (ids cuerpo)>
;
;            := evaluar <expresion> ({<expresion>}*(,)) finEval
;               <app-exp exp exps>
;
;
;<primitiva-booleana> := < (primitiva-booleana-menor)
;
;                     := > (primitiva-booleana-mayor)
;
;                     := <= (primitiva-booleana-menor-igual)
;
;                     := >= (primitiva-booleana-mayor-igual)
;
;                     := == (primitiva-booleana-igual)
;
;                     := != (primitiva-booleana-diferente)
;
;                     := && (primitiva-booleana-and)
;
;                     := ^^ (primitiva-booleana-or)
;
;                     := not (primitiva-booleana-not)
;
;
;<primitiva-aritmetica> :=  + (primitiva-suma)
;
;                       :=  - (primitiva-resta)
;
;                       :=  / (primitiva-div)
;
;                       :=  * (primitiva-multi)
;
;                       := add1 (primitiva-add1)
;
;                       := sub1 (primitiva-subb1)
;
;
;<primitiva-aritmetica-base> :=   +  (primitiva-base-suma)
;
;                            :=   -  (primitiva-base-resta)
;
;                            :=   *  (primitiva-base-multi)
;
;                            := add1 (primitiva-base-add1)
;
;                            := sub1 (primitiva-base-subb1)
;
;
;<primitiva-cadena> := longitud   (primitiva-longitud)
;
;                   := concatenar (primitiva-concatenar)
;
;
;***********************************************************************************