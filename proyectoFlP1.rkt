#lang eopl

;; PROYECTO FLP
;;  LENGUAJE MEZCLA: PYTHON, JAVA Y C++
;;  PRESENTADO POR :  Ana Acuña Mercado 1760380 - 3743
;;                    Helen Bonilla Gonzalez 1910021 - 3743
;;                    Hanier Peña Arias 1741594 - 3743
;;                    Juan Esteban Camargo Chacón 1924984 - 3743


;; Definicion BNF para las expresiones del lenguaje:
;;
;; <programa>            ::=<expresion>
;;                         programa (exp)
;;
;; <identificador>       ::={<expresion>}
;;                         identificador (exps)
;;
;; <expresion>           ::=<variable>
;;                          var-exp (id)

;;                       ::=<numero>
;;                          numb-exp (num)

;;                       ::= <true>
;;                          true-exp (true)

;;                       ::= <false>
;;                          false-exp (false)

;;                       ::=<boolean>
;;                          bool-type-exp (bool) 

;;                       ::=<hexadecimal>
;;                          hex-type-exp (hex)

;;                       ::=<octal>
;;                          oct-type-exp (oct)

;;                       ::=<base32>
;;                          bas-type-exp (bas)

;;                       ::=(<expresion> <primitiva-Binaria> <expresion>)
;;
;;                       ::=(<primitiva-Unaria> <expresion>)

;;                       ::=(<expresion><primitivaBol><expresion>)

;;                       ::=("[""hex" <numero> (<numero>*) ":" "]" )
;;
;;                       ::=("[""oct" <numero> (<numero>*) ":" "]" )
;;
;;                       ::=("[""bas" <numero> (<numero>*) ":" "]" )

;;                       ::(<variable> "=" "[" "(<expresion>*)" "," "]")

;;                       ::="for" <variable> "in" <expresion> ":" (<expresion>*)

;;                       ::="if" <expresion> "primitiva-bool" <expresion> ":"
;;                          (<expresion>)* ";"

;;                            "else" ":" (<expresion>)* ";"

;;                       ::=<funcion>
;;                         "def" <string> "("<identificador>")" ":" (<expresion>*)

;;
;; <primitiva-binaria>   ::= ” + ” 
;;                       ::= ” - ” 
;;                       ::= “ * ” 
;;                       ::= “ / ” 
;;                       ::= “ % ”
;;                       ::= “ append ”
;;
;;<primitiva-unaria>     ::= “ length ”
;;		         ::= “ pow ” 
;;		         ::= “print”
;;      	         ::= “++” 
;;		         ::= “--”
;;
;;
;; <primitivas-bool>     ::="<"
;;                         (menor-prim)
;;                       ::=">"
;;                         (mayor-prim)
;;                       ::="!<"
;;                         (meq-prim)
;;                       ::="!>"
;;                         (maq-prim)
;;                       ::="dif"
;;                         (difq-prim)
;;                       ::="equal"
;;                         (equal-prim)
;;                       ::="&&"
;;                         (and-prim)
;;                       ::="or"
;;                         (or-prim)
;;
;; <primitiva bool-U>    ::="not"
;;                         (not-prim)
;;
;; <primitiva-oct>       ::="+_8"
;;                         (oct-sum)
;;                       ::="-_8"
;;                         (oct-res)
;;                       ::="*_8"
;;                         (oct-mult)
;;                       ::="++_8"
;;                         (oct-incr)
;;                       ::="--_8"
;;                         (oct-decr)
;;
;; <primitiva-hex>       ::="+_16"
;;                         (hex-sum)
;;                       ::="-_16"
;;                         (hex-res)
;;                       ::="*_16"
;;                         (hex-mult)
;;                       ::="++_16"
;;                         (hex-incr)
;;                       ::="--_16"
;;                         (hex-decr)
;;
;; <primitiva-base>      ::="+_32"
;;                         (base-sum)
;;                       ::="-_32"
;;                         (base-res)
;;                       ::="*_32"
;;                         (base-mult)
;;                       ::="++_32"
;;                         (base-icr)
;;                       ::="--_32"
;;                         (base-decr)
;;
;;
;;
;; <primitiva-cad>       ::=<longitud>
;;                         length-exp(length)
;;                       ::=<concatenar>
;;                         concat-exp(concat)
;;
;; <primitiva-list>      ::="vacio?"
;;                         (vacio?)

;;                       ::="vacio"
;;                         (vacio)

;;                       ::="cons"
;;                         (cons)

;;                       ::="list?"
;;                         (list?)

;;                       ::="car"
;;                         (car)

;;                       ::="cdr"
;;                         (cdr)

;;                       ::="append"
;;                         (append)
;;
;;
;; <primitiv-vect>       ::="vector?"
;;                         (vector?)
;;                       ::="make-vector"
;;                         (make-vector)
;;                       ::="vector-ref"
;;                         (vector-ref)
;;                       ::="vector-set!"
;;                         (vector-set!)

;; <comentarios>         ::="***"
;;                         (comentar)
;;-------------------------------------------------------

;;Especificacion lexica:

(define lexica
  '((white-sp(whitespace)skip)
    (comment("***" (arbno(not #\newline)))skip) ;comentario
    (texto(letter(arbno(or letter digit)))string) ;texto
    (identificador ("$" letter (arbno (or letter digit))) symbol) ;identificador
    (numero (digit(arbno digit)) number) ;numero entero
    (numero("-" digit(arbno digit))number) ;numero negativo
    (numero(digit(arbno digit)"."digit(arbno digit))number) ;numero flotante
    (numero("-"digit(arbno digit)"."digit(arbno digit))number))) ;numero negativo flotante

;Especificación Sintáctica (gramática):

(define gramatica
  '((program (expresion) programa)
    (expresion (number) lit-exp) ;numeros enteros y flotantes
    (expresion ("[x16" (separated-list number ":") "]") hex-lit-exp) ;Hexadecimales (hex 2:12:15:10) -> AFC2
    (expresion ("[x8" (separated-list number ":") "]") oct-lit-exp) ;Octales (oct 1:3:7:5) -> 5731
    (expresion ("[x32" (separated-list number ":")"]") bas-lit-exp) ;base32 (bas 0:7:33:8) -> AH9I
    (expresion (id) var-exp) ;variables
    (expresion ("\"" id "\"") texto-lit) ;cadenas de Texto
    ;; Operaciones Primitivas con Enteros:
    (expresion (primitiva-binaria "[" expresion "," expresion "]")primapp-exp) ;Binarias +-*/%
    (expresion (primitiva-unaria "[" expression "]")unary-primapp-exp) ;; Unarias add1/sub1
    ;; Operaciones Primitivas sobre Cadenas de Texto:
    (expresion (text-primitiva "(" expresion "," expresion ")")text-primapp-exp) ;Binaria
    (expresion (text-primitiva-u "(" expresion ")")text-primapp-exp-u) ;Unarias
    ;; Operaciones primitivas booleanas:
    (expresion (bool-primitiva "(" expresion ";" expresion ")")bool-primapp-exp) ;Binarias
    (expresion (bool-primitiva-u "(" expresion ")")bool-primapp-exp-u) ;Unarias
    ;; Operaciones primitivas sobre Hexadecimales/Octales:
    (expresion (ho-primitiva "{" expresion "," expresion "}")hexoct-primapp-exp) ;BINARIAS
    (expresion (ho-primitiva-u "{" expresion "}")hexoct-primapp-exp-u) ;UNARIAS    
    (expresion ("false") false-exp) ;False Booleano
    (expresion ("true") true-exp)  ;True Booleano
    (expresion ("if" expresion "then" expresion "else" expresion)if-exp) ;Condicionales
    (expresion ("declare" (arbno id "=" expresion) "in" expresion)let-exp) ;Declaración (ligadura) de variable
    (expresion ("function" "(" (separated-list type-exp id ",") ")" "{"expresion"}")proc-exp) ;Procedimientos
    (expresion ("[" expresion (arbno expresion) "]")app-exp) ;Invocación de procedimientos
    (expresion ("recursiva" (arbno type-exp id"(" (separated-list type-exp id ",") ")""=" expresion)  "in" expresion) letrec-exp) ;Declaración recursiva
    (expresion ("for" (arbno id "in" expresion ":" expresion ))for-expresion);Ciclos For
    ;; Bloque de ejecuciones:
    (expresion ("initBlock" expresion (arbno ";" expresion) "end")block-exp)
    ;; Asignación de valores:
    (expresion ("assign" id "=" expresion)set-exp)
    ;; Primitivas aritméticas para enteros:
    (primitiva-binaria ("+") add-prim)        ; Suma
    (primitiva-binaria ("-") substract-prim)  ; Resta
    (primitiva-binaria ("*") mult-prim)       ; Multiplicación
    (primitiva-binaria ("/") div-prim)        ; División
    (primitiva-binaria ("%") modu-prim)     ; Modulo
    (primitiva-unaria ("add") incr-prim)   ; Sumar 1
    (primitiva-unaria ("sub") decr-prim)   ; Restar 1
    ;; Primitivas para cadenas de Texto:
    (text-primitiva ("concat") concat-prim)    ; Concatenar
    (text-primitiva-u ("length") length-prim) ; Longitud
    ;; Primitivas Booleanas:
    (bool-primitiva (">") mayor-prim)      ; Mayor-que
    (bool-primitiva ("<") menor-prim)      ; Menor-que
    (bool-primitiva ("¡>") mayoreq-prim)   ; Mayor-igual
    (bool-primitiva ("!<") menoreq-prim)   ; Menor-igual
    (bool-primitiva ("equ") equal-prim)    ; Igual
    (bool-primitiva ("dif") diff-prim)     ; Diferente
    (bool-primitiva ("&&") and-prim)       ; And-lógico
    (bool-primitiva ("or") or-prim)        ; Or-lógico
    (bool-primitiva-u ("not") not-prim)    ; Negación-lógica
    ;; Primitivas Hexadecimales/Octales/Base32:
    (primitiva-hex ("+_16") hex-oct-bas-add-prim)      ; Suma
    (primitiva-hex ("-_16") hex-oct-bas-sub-prim)      ; Resta
    (primitiva-hex ("*_16") hex-oct-bas-mult-prim)     ; Multiplicación
    (primitiva-hex ("++_16") hex-oct-bas-incr-prim)    ; Incrementar
    (primitiva-hex ("--_16") hex-oct-bas-decr-prim)    ; Decrementar
    ;; Primitivas Octales:
    (primitiva-oct ("+_8") hex-oct-bas-add-prim)      ; Suma
    (primitiva-oct ("-_8") hex-oct-bas-sub-prim)      ; Resta
    (primitiva-oct ("*_8") hex-oct-bas-mult-prim)     ; Multiplicación
    (primitiva-oct ("++_8") hex-oct-bas-incr-prim)    ; Incrementar
    (primitiva-oct ("--_8") hex-oct-bas-decr-prim)    ; Decrementar
    ;; Primitivas Base32:
    (primitiva-base ("+_32") hex-oct-bas-add-prim)      ; Suma
    (primitiva-base ("-_32") hex-oct-bas-sub-prim)      ; Resta
    (primitiva-base ("*_32") hex-oct-bas-mult-prim)     ; Multiplicación
    (primitiva-base ("++_32") hex-oct-bas-incr-prim)    ; Incrementar
    (primitiva-base ("--_32") hex-oct-bas-decr-prim)    ; Decrementar
    ))


;;=========================================================================================
;; Construcción de los Datatypes y creación del prompt (SLLG

;; El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse (sllgen:make-string-parser lexica gramatica))
