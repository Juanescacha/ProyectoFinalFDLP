#lang eopl
(require racket/string)

;; Integrantes: 
;; Juan David Morán Bonilla - 201810052

;; Proyecto del Curso 2018-2
;; Fundamentos de Lenguajes de Programación

; -> Segunda Parte: Ruby con Objetos

;Especificación Léxica
(define scanner-spec-simple-interpreter
'((white-sp (whitespace) skip)
  (comment ("#" (arbno (not #\newline))) skip)
  (identifier ((arbno "@") letter (arbno (or letter digit "_" "?" "="))) symbol)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (text ("\"" (or letter whitespace)
              (arbno (or letter digit whitespace ":" "?" "=" "'")) "\"") string)))

;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '((ruby-program ("ruby" (arbno class-decl) exp-batch "end") a-program) ;Característica adicional
    
    (exp-batch (expression (arbno expression)) a-batch)

    (expression (simple-exp) a-simple-exp)
    (expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
    (expression ("puts" (separated-list comp-value ",") ";") puts-exp)
    (expression ("if" comp-value (arbno "then") exp-batch
                      (arbno "elsif" comp-value (arbno "then") exp-batch)
                      (arbno "else" exp-batch) "end") if-exp)
     (expression ("unless" comp-value (arbno "then") exp-batch
                      (arbno "else" exp-batch) "end") unless-exp)
     (expression ("while" comp-value (arbno "do") exp-batch "end") while-exp)
     (expression ("until" comp-value (arbno "do") exp-batch "end") until-exp)
     (expression ("for" identifier "in" comp-value (arbno "do") exp-batch "end") for-exp)
     (expression ("def" identifier "(" (separated-list identifier ",") ")" exp-batch "end") function-exp)
     (expression ("return" comp-value ";") return-exp)

     (expression ("super" identifier "(" (separated-list comp-value ",") ")" ";") super-exp) ;Característica adicional
     
     (simple-exp (simple-value complement ";") val-exp)

     (complement ("=" comp-value calls) assign)
     (complement (assign-op comp-value calls) assign-and)
     (complement (calls) comp-calls)

     (calls ((arbno call)) some-calls)    
     
     (call (arguments) arguments-call)
     (call ("." identifier arguments) a-method-call) ;Característica adicional
     
     (arguments ("(" (separated-list comp-value ",") ")") some-arguments)
     (arguments ("[" comp-value (arbno "," comp-value) "]") arr-arguments)

     (comp-value (value) a-value)
     (comp-value (un-op comp-value) unop-value)

     (value (simple-value) a-s-val)
     (value ("(" comp-value val-compl ")") compl-val)
     
     (val-compl (calls) val-call)
     (val-compl (bin-op comp-value) binop-val)
     
     (simple-value (identifier) id-val)
     (simple-value (number) num-val)
     (simple-value (text) str-val)
     (simple-value ("true") true-val)
     (simple-value ("false") false-val)
     (simple-value ("nil") nil-val)
     (simple-value ("[" (separated-list comp-value ",") "]") arr-val)

     ;Característica adicional
     (class-decl ("class" identifier
                          (arbno "<" identifier)
                          "attr" (separated-list ":" identifier ",") ";"
                          (arbno method-decl) "end") a-class-decl)

     (method-decl ("def" identifier "(" (separated-list identifier ",") ")" exp-batch "end") a-method-decl) ;Característica adicional
     
     (bin-op ("+") add)
     (bin-op ("-") diff)
     (bin-op ("*") mult)
     (bin-op ("/") div)
     (bin-op ("%") mod)
     (bin-op ("**") pow)
     (bin-op (">") great)
     (bin-op (">=") great-eq)
     (bin-op ("<") less)
     (bin-op ("<=") less-eq)
     (bin-op ("==") is-equal)
     (bin-op ("!=") not-equal)
     (bin-op ("and") and-op)
     (bin-op ("&&") and-op)
     (bin-op ("or") or-op)
     (bin-op ("||") or-op)
     (bin-op ("..") in-range)
     (bin-op ("...") ex-range)
     (bin-op ("step") st-range)

     (assign-op ("+=") add-eq)
     (assign-op ("-=") diff-eq)
     (assign-op ("*=") mult-eq)
     (assign-op ("/=") div-eq)
     (assign-op ("**=") pow-eq)
     
     (un-op ("not") not-op)
     (un-op ("!") not-op)))


;Tipos de datos para la sintaxis abstracta de la gramática
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;Análisis léxico y Sintáctico
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador
(define interpretador
  (sllgen:make-rep-loop  "=> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;Función que evalúa un programa teniendo en cuenta un ambiente inicial
(define eval-program
  (lambda (pgm)
    (cases ruby-program pgm
      (a-program (c-decls a-batch)
                 (elaborate-class-decls! c-decls)
                 (eval-exp-batch a-batch (init-env))))))

;Ambiente inicial
(define (init-env) (empty-env))

;Función que evalua un lote de expresiones
(define eval-exp-batch
  (lambda (abh env)
    (cases exp-batch abh
      (a-batch (exp exps)
               (eval-a-batch exp exps env))))) 

;Función que aplica el respectivo proceso a cada diferente expresión
(define eval-a-batch
  (lambda (exp exps env)
    (cases expression exp
      (a-simple-exp (simple-exp)
                  (if (null? exps)
                      (eval-simple-exp simple-exp env)
                      (begin
                        (eval-simple-exp simple-exp env)
                        (eval-a-batch (car exps) (cdr exps) env))))
      (declare-exp (id ids)
                   (let ((ls-ids (append (list id) ids)))
                     (let ((env-extend (extend-env ls-ids (declare-args-aux (length ls-ids) '()) env)))
                       (if (null? exps)
                           'nil
                           (eval-a-batch (car exps) (cdr exps) env-extend)))))
      (puts-exp (vals)
                (if (null? exps)
                    (let ((result (map (lambda (val) (eval-comp-value val env)) vals)))
                      (for-each (lambda (arg)
                                  (eopl:pretty-print arg))
                                result) 'nil)
                    (begin
                      (let ((result (map (lambda (val) (eval-comp-value val env)) vals)))
                        (for-each (lambda (arg)
                                    (eopl:pretty-print arg))
                                  result) 'nil)
                      (eval-a-batch (car exps) (cdr exps) env))))
      (if-exp (a-comp-value then-exp else-ifs elsif-then else)
              (if (null? exps)
                  (let ((value-result (eval-comp-value a-comp-value env)))
                    (if (equal? #t value-result)
                        (eval-exp-batch then-exp env)
                        (eval-if-exp else-ifs elsif-then else env)))
                  (begin
                    (let ((value-result (eval-comp-value a-comp-value env)))
                      (if (equal? #t value-result)
                          (eval-exp-batch then-exp env)
                          (eval-if-exp else-ifs elsif-then else env)))
                    (eval-a-batch (car exps) (cdr exps) env))))
      (unless-exp (a-comp-value then-exp else)
                  (if (null? exps)
                      (let ((value-result (eval-comp-value a-comp-value env)))
                        (if (equal? #f value-result)
                            (eval-exp-batch then-exp env)
                            (if (null? else)
                                'nil
                                (eval-exp-batch (car else) env))))
                      (begin
                        (let ((value-result (eval-comp-value a-comp-value env)))
                          (if (equal? #f value-result)
                              (eval-exp-batch then-exp env)
                              (if (null? else)
                                  'nil
                                  (eval-exp-batch (car else) env))))
                        (eval-a-batch (car exps) (cdr exps) env))))
      (while-exp (a-comp-value body)
                 (let ((value-result (eval-comp-value a-comp-value env)))
                   (if (equal? #t value-result)
                       (begin
                         (eval-exp-batch body env)
                         (eval-a-batch exp exps env))
                       (if (null? exps)
                           'nil
                           (eval-a-batch (car exps) (cdr exps) env)))))
      (until-exp (a-comp-value body)
                 (let ((value-result (eval-comp-value a-comp-value env)))
                   (if (equal? #f value-result)
                       (begin
                         (eval-exp-batch body env)
                         (eval-a-batch exp exps env))
                       (if (null? exps)
                           'nil
                           (eval-a-batch (car exps) (cdr exps) env)))))
      (for-exp (id a-range body)
               (if (null? exps)                     
                   (let ((result (eval-comp-value a-range env)))
                         (eval-for-exp id result  body (extend-env (list id) (list 'nil) env)))
                   (begin
                     (let ((result (eval-comp-value a-range env)))
                           (eval-for-exp id result  body (extend-env (list id) (list 'nil) env)))
                     (eval-a-batch (car exps) (cdr exps) env))))
      (function-exp (proc-name idss body)
                    (if (null? exps)
                        proc-name
                        (eval-a-batch (car exps) (cdr exps) (a-recursive-env proc-name idss body env))))
      (return-exp (a-comp-value)
                  (if (null? exps)
                      (eval-comp-value a-comp-value env)
                      (begin
                        (eval-comp-value a-comp-value env)
                        (eval-a-batch (car exps) (cdr exps) env))))

      ;Característica adicional
      (super-exp (method-name args)
                 (let ((list-args (just-eval-arguments args env))
                       (obj (apply-env env 'self)))
                   (find-method-and-apply
                    method-name (apply-env env '%super) obj list-args))))))

;Función que se encarga de las asignaciones, llamados a métodos (o posición de un arreglo) y una operación unitaria
(define eval-simple-exp
  (lambda (s-exp env)
    (cases simple-exp s-exp
      (val-exp (a-simple-value a-complement)(apply-complement (aux-eval-simple-exp a-simple-value) a-complement env)))))

;Función que retorna la mínima representación de un dato
(define eval-simple-value
  (lambda (s-val env)
    (cases simple-value s-val
      (id-val (id)(let ((aux (lookup-class id)))
                    (if (equal? 'vacio aux)
                        (apply-env env id)
                        aux)))
      (num-val (val) val)
      (str-val (val) (string-trim val "\""))
      (true-val () #t)
      (false-val () #f)
      (nil-val () empty)
      (arr-val (vals)(map (lambda (val) (eval-comp-value val env)) vals)))))

;Función que aplica la asignación, una operación y una asignación o el llamado de una función en relación a unos argumentos ingresados
(define apply-complement
  (lambda (s-val compl env)
    (cases complement compl
      (assign (a-comp-value call-list)
             (cases calls call-list
               (some-calls (list-call)
                          (if (> (length list-call) 0)
                             (begin
                               (setref!
                                (apply-env-ref env s-val)
                                (apply-call-list (eval-comp-value a-comp-value env) list-call env))
                               'nil)
                             (begin
                               (setref!
                                (apply-env-ref env s-val)
                                (eval-comp-value a-comp-value env))
                               'nil)))))
      (assign-and (an-op a-comp-value call-list)
                 (let ((comp-value
                        (cases calls call-list
                          (some-calls (list-call)
                                     (if (> (length list-call) 0)
                                        (apply-call-list (eval-comp-value a-comp-value env) list-call env)
                                        (eval-comp-value a-comp-value env)))))
                       (val (apply-env env s-val)))
                   (cond
                     ((eq? 'nil val)(eopl:error 'Error "No definition for ~a" s-val))
                     ((not (number? val))(eopl:error 'Error "Expected numeric argument"))
                     (else
                      (cases assign-op an-op
                        (add-eq ()
                                (begin
                                  (setref!
                                   (apply-env-ref env s-val)
                                   (+ val comp-value))
                                  'nil))
                        (diff-eq ()
                                 (begin
                                   (setref!
                                    (apply-env-ref env s-val)
                                    (- val comp-value))
                                   'nil))
                        (mult-eq ()
                                 (begin
                                   (setref!
                                    (apply-env-ref env s-val)
                                    (* val comp-value))
                                   'nil))
                        (div-eq ()
                                (begin
                                  (setref!
                                   (apply-env-ref env s-val)
                                   (/ val comp-value))
                                  'nil))
                        (pow-eq ()
                                (begin
                                  (setref!
                                   (apply-env-ref env s-val)
                                   (expt val comp-value))
                                  'nil)))))))
      (comp-calls (some-call)
                  (cases calls some-call
                    (some-calls (list-call)
                                (apply-call-list (apply-env env s-val) list-call env)))))))

;Función que evalua la negación de una operación o de un valor compuesto
(define eval-comp-value
  (lambda (c-val env)
  (cases comp-value c-val
    (a-value (val) (eval-value val env))
    (unop-value (un-op val)
                (let ((result (eval-comp-value val env)))
                 (if (equal? #t result) #f #t))))))

;Función que aplica la correspondiente aplicación a un valor simple o un valor compuesto
(define eval-value
  (lambda (a-val env)
    (cases value a-val
      (a-s-val (a-simple-value) (eval-simple-value a-simple-value env))
      (compl-val (c-val a-val-compl) (eval-val-compl (eval-comp-value c-val env)  a-val-compl env)))))

;Función que evalua el complemento de un valor, ya sea una llamada o una operación bin-op
(define eval-val-compl
  (lambda (a-val a-val-compl env)
    (cases val-compl a-val-compl
      (val-call(some-call)
               (cases calls some-call
                 (some-calls (list-call) (apply-call-list a-val list-call env))))
      (binop-val(binop val) (eval-binop binop a-val (eval-comp-value val env))))))

;Función que evalua una lista de llamadas con sus correspondientes validaciones
(define apply-call-list
  (lambda (a-value c-list env)
    (cond
      ((or (number? a-value) (string? a-value) (boolean? a-value)) (eopl:error 'Error "Can’t apply args to ~a" a-value))
      ((and (= 2 (length c-list)) (or (call-aux (car c-list)) (call-aux (cadr c-list)))) ;Si se hacen dos llamadas a procedimientos. Ejemplo: add1()()
       (eopl:error 'Error "Arguments can't be a procedure"))
      ((= 1 (length c-list))(apply-call a-value (car c-list) env))
      ((= 2 (length c-list))
       (let ((result-call (apply-call a-value (car c-list) env)))
         (apply-call result-call (cadr c-list) env)))
      (else
       (eopl:error 'Error "Data entered is not a variable")))))

;Función que procesa una llamada a un procedimento o a la posición de arreglo
(define apply-call
  (lambda (a-value a-call env)
    (cases call a-call
      (arguments-call (args) (apply-arguments a-value args env))
      (a-method-call (method-name args)
                     (if (equal? method-name 'new)
                         (let ((list-args (eval-arguments args env))
                               (obj (new-object (class->class-name a-value))))
                           (find-method-and-apply
                            'initialize (class->class-name a-value) obj list-args) obj)
                         (let ((list-args (eval-arguments args env)))
                           (find-method-and-apply
                            method-name (class->class-name (object->class-decl a-value)) a-value list-args)))))))

;Función que emplea los argumentos y el valor ingresado para procesar la llamada
(define apply-arguments
  (lambda (a-val args env)
    (cases arguments args
      (some-arguments (args) (apply-procedure a-val (just-eval-arguments args env) env))
      (arr-arguments (arg list-args)
                     (let ((args-aux (just-eval-arguments (cons arg list-args) env)))
                       (cond
                          ;Cuando se toma una posición del arreglo
                         ((and (= 1 (length args-aux)) (not (number? (car args-aux)))) (eopl:error 'Error "Expected numerical argument")) ;Se valida que el argumento sea un número
                         ((and (= 1 (length args-aux)) (< (car args-aux) 0)) (eopl:error 'Error "Expected positive argument")) ;Se valida que el argumento sea entero positivo
                         ((and (= 1 (length args-aux)) (> (car args-aux) (- (length a-val) 1))) (eopl:error 'Error "Expected first argument smaller than the size of the arrangement")) ;Se valida que el argumento sea menor al tamaño del arreglo
                         ((= 1 (length args-aux)) (arrangement-aux a-val (car args-aux) 0))
                         
                         ;Cuando se toma más de una posición del arreglo
                         ((and (= 2 (length args-aux))(or (< (car args-aux) 0) (< (cadr args-aux) 0))) ;Se valida que los argumentos del rango no sean negativos 
                          (eopl:error 'Error "Expected 1 to 2 positive arguments"))
                         ((and (= 2 (length args-aux))(> (car args-aux) (cadr args-aux))) ;Se valida que el primer argumento sea menor que el segundo
                          (eopl:error 'Error "Expected first argument smaller than the second argument"))
                         ((and (= 2 (length args-aux))(or (> (cadr args-aux) (length a-val)) (> (car args-aux) (length a-val)))) ;Se valida que el segundo argumento no sobrepase el tamaño del arreglo
                          (eopl:error 'Error "Expected second argument smaller than the size of the arrangement"))
                         ((= 2 (length args-aux)) (arrangement-range-aux a-val (car args-aux) (cadr args-aux) 0 (car args-aux)))

                         (else
                          (eopl:error 'Error "Expected 1 to 2 positive arguments"))))))))

;Función que retorna la lista de los argumentos ingresados ya evaluados 
(define just-eval-arguments 
  (lambda (args env)
    (if (null? args)
        '()
        (append (list (eval-comp-value (car args) env)) (just-eval-arguments (cdr args) env)))))

;Función que evalua una operación respecto a dos valores ingresados 
(define eval-binop
  (lambda (binop val1 val2)
    (cond
      ((and (boolean? val1) (number? val2))(eopl:error 'Error "Not defined method <bin-op> for boolean"))
      ((and (number? val1) (boolean? val2))(eopl:error 'Error "Boolean can’t be coerced into Integer"))
      ((and (number? val1) (list? val2))(eopl:error 'Error "Array can’b be coerced into Integer"))
      (else
       (cases bin-op binop
         (add ()
              (cond
                ((and (number? val1) (number? val2))(+ val1 val2))
                ((and (string? val1) (string? val2))(string-append val1 val2))
                ((and (string? val1) (number? val2))(eopl:error 'Error "No implicit conversion of Integer into String"))
                ((and (number? val1) (string? val2))(eopl:error 'Error "String can’t be coerced into Integer"))
                ((and (list? val1) (list? val2))(append val1 val2))
                (else (eopl:error 'Error "Type of data entered not valid"))))
         (diff () (if (and (number? val1) (number? val2))
                      (- val1 val2)
                      (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))         
         (mult ()
               (cond
                 ((and (string? val1) (number? val2))(mult-string val1 val2 ""))
                 ((and (number? val1) (number? val2))(* val1 val2))
                 ((and (list? val1) (number? val2))(mult-arr val1 val2 '()))
                 (else (eopl:error 'Error "Type of data entered not valid"))))
         (div () (if (and (number? val1) (number? val2))
                     (/ val1 val2)
                     (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (mod () (if (and (number? val1) (number? val2))
                     (modulo val1 val2)
                     (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (pow () (if (and (number? val1) (number? val2))
                     (expt val1 val2)
                     (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (great () (if (and (number? val1) (number? val2))
                       (> val1 val2)
                       (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (great-eq ()
                   (if (and (number? val1) (number? val2))
                       (>= val1 val2)
                       (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (less ()
               (if (and (number? val1) (number? val2))
                   (< val1 val2)
                   (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (less-eq ()
                  (if (and (number? val1) (number? val2))
                      (<= val1 val2)
                      (if(and (list? val1) (number? val2))
                         (eopl:error 'Error "No implicit conversion of Integer into Array")
                         (eopl:error 'Error "Type of data entered not valid"))))
         (is-equal () (if (equal? val1 val2) #t #f))
         (not-equal () (if (equal? val1 val2) #f #t))
         (and-op () (and val1 val2))
         (or-op () (or val1 val2))
         (in-range ()
                   (cond
                     ((or (not (number? val1)) (not (number? val2))) (eopl:error 'Error "Type of data entered not valid"))
                     ((< val2 val1) (eopl:error 'Error "Expected first argument smaller than the second argument"))
                     (else (inclusive val1 val2 0))))
         (ex-range ()
                   (cond
                     ((or (not (number? val1)) (not (number? val2))) (eopl:error 'Error "Type of data entered not valid"))
                     ((< val2 val1) (eopl:error 'Error "Expected first argument smaller than the second argument"))
                     (else (exclusive val1 (- val2 1) 0))))
         (st-range ()
                   (cases range val1
                     (inclusive (start end step)(if (< val2 0)
                                                    (eopl:error 'Error "Expected second argument greater than zero")
                                                    (inclusive start end val2)))
                     (exclusive (start end step)(if (< val2 0)
                                                    (eopl:error 'Error "Expected second argument greater than zero")
                                                    (exclusive start end val2))))))))))

;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;Función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))

;Función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;Función que crea un ambiente extendido por referencia
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;Función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define (a-recursive-env a-proc-name ids body env)
  (let ((vec (make-vector 1)))
    (let ((env (extended-env-record (list a-proc-name) vec env)))
          (vector-set! vec 0 (closure ids body env))
          env)))

;Función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'Error "Undefined local variable or method ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;Referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;Rangos
(define-datatype range range?
  (inclusive (start number?) (end number?) (step number?))
  (exclusive (start number?) (end number?) (step number?)))

;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body exp-batch?)
   (env environment?)))

;Función que evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
               (eval-exp-batch body (extend-env ids args env))))))

;Ambientes de las Clases

;Funciones que inicializan el ambiente de las clases
(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

;Función que agrega una clase al ambiente de las clases
(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

;Función que busca una clases en el ambiente de las clases
(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) 'vacio)
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;Clases
(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

;Funciones que construyen las clases ingresadas
(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl super-name field-ids)))))))

(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))

;Declaraciones para el uso de las clases

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    (if (null? super-name)
                        'object
                        (car super-name))))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;Objetos

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
     (make-vector
      (class-name->field-length class-name)))))

;Métodos de las clases

(define-datatype method method?
  (a-method
   (method-decl method-decl?)
   (super-name symbol?)
   (field-ids (list-of symbol?))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name 'object)
          (eopl:error 'find-method-and-apply
                      "No method for name ~s" m-name)
          (let ((method (lookup-method m-name
                                       (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (class-name->super-name host-name))))))))

(define apply-method
  (lambda (method host-name self args)
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (eval-exp-batch body
                       (extend-env
                        (cons '%super (cons 'self ids))
                        (cons super-name (cons self args))
                        (extend-env-refs field-ids fields (empty-env)))))))

;Funciones que ayudan a gestionar el procesamiento de las clases

(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
               (if (null? super-name)
                   'object
                   super-name)))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))

;Ambiente de los métodos de las clases

(define method-environment? (list-of method?)) 

(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;Funciones Auxiliares

;Funciones auxiliares para encontrar la posición de un símbolo en la lista de símbolos de un ambiente
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

;Función que retorna una lista de nil's
(define declare-args-aux
  (lambda (size result)
    (if (= size 0)
        result
        (declare-args-aux (- size 1) (cons 'nil result)))))

;Función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;Función que concatena el string str n veces
(define mult-string
  (lambda (str n result)
    (if(= n 0)
       result
       (mult-string str (- n 1) (string-append result str)))))


;Función que multiplica n veces el arreglo ls
(define mult-arr
  (lambda (ls n result)
    (if (= n 0)
        result
        (mult-arr ls (- n 1) (append result ls)))))

;Función que evalua las expresiones elseif y el else de if-exp
(define eval-if-exp
  (lambda (if-list then-list else-batch env)
    (if (and (null? if-list) (null? then-list))
        (if (null? else-batch)
            'nill
            (eval-exp-batch (car else-batch) env))
        (if (eval-comp-value (car if-list) env)
            (eval-exp-batch (car then-list) env)
            (eval-if-exp (cdr if-list) (cdr then-list) else-batch env)))))

;Función que evalua una expresión for-exp
(define eval-for-exp
  (lambda (id a-range body env)
   (if (list? a-range)
       (if (null? a-range)
           'nil
           (begin
             (setref!
              (apply-env-ref env id)
              (car a-range))
             (eval-exp-batch body env)
             (eval-for-exp id (cdr a-range) body env)))
       (cases range a-range
         (inclusive (start end step)
                    (if (<= start end)
                        (if (zero? step)
                            (begin
                              (setref!
                               (apply-env-ref env id)
                               start)
                              (eval-exp-batch body env)
                              (eval-for-exp id (inclusive (+ 1 start) end step) body env))
                            (begin
                              (setref!
                               (apply-env-ref env id)
                               start)
                              (eval-exp-batch body env)
                              (eval-for-exp id (inclusive (+ step start) end step) body env)))
                        'nil))
         (exclusive (start end step)
                    (if (<= start end)
                        (if (zero? step)
                            (begin
                              (setref!
                               (apply-env-ref env id)
                               start)
                              (eval-exp-batch body env)
                              (eval-for-exp id (exclusive (+ 1 start) end step) body env))
                            (begin
                              (setref!
                               (apply-env-ref env id)
                               start)
                              (eval-exp-batch body env)
                              (eval-for-exp id (exclusive (+ step start) end step) body env)))
                        'nil))))))

;Función que recibe un valor simple y retorna el nombre del id
(define aux-eval-simple-exp
  (lambda (sv)
    (cases simple-value sv
      (id-val (id) id)
      (else (eopl:error 'Error "Data entered is not a variable")))))

;Función que recibe un arreglo, un punto de inicio y de final. Da como resultado la sección del arreglo solicitado
(define arrangement-range-aux
  (lambda (ls start end cont cont1)
    (if (= cont start)
        (if (= cont1 end)
            '()
            (append (list (car ls)) (arrangement-range-aux (cdr ls) start end cont (+ 1 cont1))))
        (arrangement-range-aux (cdr ls) start end (+ 1 cont) cont1))))

;Función que recibe un arreglo, una posición y un contador. Da como resultado lo contenido en la posición solicitada
(define arrangement-aux
  (lambda (ls position cont)
    (if(= position cont)
       (car ls)
       (arrangement-aux (cdr ls) position (+ 1 cont)))))

;Función que determina si la llamada hace referencia a un procedimiento (true) o a un arreglo (false)
(define call-aux
  (lambda (a-call)
    (cases call a-call
      (a-method-call (method-name args) 'metodo)
      (arguments-call (args)
                      (cases arguments args
                        (some-arguments (args) #t)
                        (arr-arguments (arg list-args) #f))))))

;Función que evalua un tipo de argumento que utiliza los call 
(define eval-arguments
  (lambda (args env)
    (cases arguments args
      (some-arguments (arg) (just-eval-arguments arg env))
      (else (eopl:error 'Error "Bad syntax")))))

(interpretador)