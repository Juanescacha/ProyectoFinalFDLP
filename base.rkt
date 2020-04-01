#lang eopl
(require racket/string)    ;Importamos la libreria para usar funciones de manejo de strings
(require racket/math)      ;Importamos la libreria math solamente para usar la definicion de pi

; Integrantes:
; Juan Esteban Camargo Chacon - 1924984
; Brayan Rincones - 1673847
; Sebastian Saldaña - 1623447


;<programa> :=  <expresion>
;
;               un-programa (exp)
;
;
;<expresion> := <numero>
;               numero-lit (num)
;
;            := "\"" <texto> "\""
;               <texto-lit (txt)>
;
;            := <identificador>
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
;<primitiva-binaria> :=  + (primitiva-suma)
;
;                    :=  ~ (primitiva-resta)
;
;                    :=  / (primitiva-div)
;
;                    :=  * (primitiva-multi)
;
;                    :=  concat (primitiva-concat)
;
;
;<primitiva-unaria>:=  longitud (primitiva-longitud)
;
;                  :=  add1 (primitiva-add1)
;
;                  :=  sub1 (primitiva-sub1)

;***********************************************************************************

; DEFINICION LEXICA Y GRAMATICA

(define lexica
  '(
    (espacios (whitespace) skip)
    (cometarios ("%" (arbno (not #\newline))) skip)
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)
    
    ; enteros
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    
    ; decimales
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)

    ; texto
    (text ("\"" (or letter whitespace) (arbno (or letter digit whitespace ":" "?" "=" "'" )) "\"") string)
    )
  )

(define gramatica
  '(
    ; programa
    (programa (expresion) un-programa)
    
    ; expresion
    (expresion (numero) numero-lit)
    (expresion (text) texto-lit)
    (expresion (identificador) var-exp)
    (expresion ( "(" expresion primitiva-binaria expresion ")" ) primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")" ) primapp-un-exp)
    (expresion ( "Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-exp)
    (expresion ( "evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") evaluar-exp)
    (expresion ( "letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion) letrec-exp)
    
    ; primitiva-binaria
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    
    ; primitiva-unaria
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
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
                        (lambda (pgm) (eval-program pgm))
                        (sllgen:make-stream-parser lexica gramatica)))

;***********************************************************************************

; EL INTERPRETE

; Funcion Evaluar Programa

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                   (eval-expression body (init-env))))))

; Funcion Evaluar Expresion

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      
      (numero-lit (num) num)
      
      (var-exp (id) (buscar-variable env id))
      
      (primapp-bin-exp (exp1 prim-binaria exp2) (apply-primapp-bin (eval-expression exp1 env) prim-binaria (eval-expression exp2 env)))
      
      (primapp-un-exp (prim-unaria exp1) (apply-primapp-un prim-unaria (eval-expression exp1 env)))
      
      (texto-lit (text) (string-trim text "\"" ))

      (condicional-exp (test-exp true-exp false-exp) (if (true-value? (eval-expression test-exp env))
                                    (eval-expression true-exp env)
                                    (eval-expression false-exp env)))
      
      (variableLocal-exp (ids exps cuerpo) (let ((args (eval-rands exps env))) (eval-expression cuerpo (extend-env ids args env))))
      
      (procedimiento-exp (ids cuerpo) (cerradura ids cuerpo env))
      
      (evaluar-exp (exp exps) (let
                                 ((proc (eval-expression exp env))
                                  (args (eval-rands exps env)))
                                 (if (procval? proc)
                                     (apply-procedure proc args)
                                     (eopl:error 'eval-expression "Intento aplicar a un no precidimiento ~s" proc))))

      (letrec-exp (proc-names ids bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names ids bodies env)))
      )))

; Funcion Evaluar Primitiva Binaria
; aplica la primitiva binaria a dos expresiones evaluando la sintaxis abstracta con cases

(define apply-primapp-bin
  (lambda (a operador b)
    (cases primitiva-binaria operador
      (primitiva-suma   () (+ a b))
      (primitiva-resta  () (- a b))
      (primitiva-multi  () (* a b))
      (primitiva-div    () (/ a b))
      (primitiva-concat () (string-append a b))
      )))

; Funcion Evaluar Primitiva Unaria
;aplica la primitiva unaria a la expresion evaluando la sintaxis abstracta con cases

(define apply-primapp-un
  (lambda (operador a)
    (cases primitiva-unaria operador
      (primitiva-longitud () (string-length a))
      (primitiva-add1     () (+ a 1))
      (primitiva-sub1     () (- a 1))
       )))

;***********************************************************************************

; AMBIENTES

; Definimos el datatype de ambiente

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                      (vals (list-of scheme-value?))
                      (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (ids (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

; creamos un ambiente Vacio

(define empty-env
  (lambda ()
    (empty-env-record)))

; creamos un ambiente extendido

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

; creamos el ambiente extendido recursivo

(define extend-env-recursively
  (lambda (proc-names ids bodies old-env)
    (recursively-extended-env-record proc-names ids bodies old-env)))

; creamos el Ambiente Inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e @pi)
      (list 1 2 3 "hola" "FLP" pi)
      (empty-env))))

; Definimos buscar-variable

(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error "Error, la variable no existe"))
      
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      
      (recursively-extended-env-record (proc-names ids bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref ids pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym)))))))


;***********************************************************************************

; FUNCIONES AUXILIARES

; Definimos list-find-position y list-find index
; Nos seriviran para encontrar la posicion de
; un simbolo en la lista de simbolos de un ambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
            (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; Definimos eval-rands y eval-rand
; Nos serviran para aplicar eval-expression a cada elemento de una lista de operandos (expresiones)

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

; definimos true-value?
; determina si un valor dado corresponde a un valor booleano FALSO o VERDADERO , todo valor es verdadero excepto el cero

(define true-value?
  (lambda (x)
    (not (zero? x))))

;***********************************************************************************

; PROCEDIMIENTOS

; Definimos el datatype proval para definir el tipo cerradura

(define-datatype procval procval?
  (cerradura
   (lista-id (list-of symbol?))
   (exp expresion?)
   (amb environment?)))

; Definimos apply-procedure que evalua el cuerpo de un procedimiento en el ambiente extendido correspondiente

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env) (eval-expression body (extend-env ids args env)))
      )))

