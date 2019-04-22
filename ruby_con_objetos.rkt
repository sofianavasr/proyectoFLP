#lang racket
(require eopl)

#|--------------------INTEGRANTES---------------------------
 Diana Sofía Navas   1629571
 Luis David Restrepo 1427086
 Walter Santacruz    1630645
 Víctor Vargas       1842274
 ----------------------------------------------------------|#

(define lexical-spec
'((white-sp (whitespace) skip)
  (comment ("#" (arbno (not #\newline))) skip)
  (identifier ((arbno "@") letter (arbno (or letter digit "_" "?" "=" ))) symbol)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (text ("\"" (or letter whitespace)
              (arbno (or letter digit whitespace ":" "?" "=" "'")) "\"") string)
  )
)

(define grammar-spec
  '( (ruby-program ("ruby" (arbno class-decl) exp-batch "end") a-program)
     (exp-batch (expression (arbno expression)) a-batch)

     ;;Expresión:
     (expression (simple-exp) a-simple-exp)
     (expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
     (expression ("puts" (separated-list comp-value ",") ";") puts-exp)
     (expression ("if" comp-value (arbno "then") exp-batch
                       (arbno "elsif" comp-value (arbno "then") exp-batch)
                       (arbno "else" exp-batch) "end") if-exp)
     (expression ("unless" comp-value (arbno "then")
                           exp-batch
                           (arbno "else" exp-batch) "end") unless-exp)
     (expression ("while" comp-value (arbno "do") exp-batch "end") while-exp)
     (expression ("until" comp-value (arbno "do") exp-batch "end") until-exp)
     (expression ("for" identifier "in" comp-value (arbno "do") exp-batch "end") for-exp)
     (expression ("def" identifier "(" (separated-list identifier ",") ")"
                  exp-batch                  
                  "end") function-exp)
     (expression ("return" comp-value ";") return-exp)

     (class-decl ("class" identifier
                          (arbno "<" identifier)
                          "attr" (separated-list ":" identifier ",") ";"
                          (arbno method-decl) "end") a-class-decl)

     (method-decl ("def" identifier "(" (separated-list identifier ",") ")"
                  exp-batch                  
                  "end") a-method-decl)

     ;;Expresión simple
     (simple-exp (simple-value complement ";") val-exp)
     
     ;;Complemento
     (complement ("=" comp-value calls) assign)
     (complement (assign-op comp-value calls) assign-and)
     (complement (calls) comp-calls)

     ;;Calls
     (calls ((arbno call)) some-calls)

     ;;Call
     (call (arguments) arguments-call)
     (call ("." identifier arguments) a-method-call)
 
     ;;Argumentos
     (arguments ("(" (separated-list comp-value ",") ")") some-arguments)
     (arguments ("[" comp-value (arbno "," comp-value) "]") arr-arguments)

     ;;Valores compuestos
     (comp-value (value) a-value)
     (comp-value (un-op comp-value) unop-value)
     
     (value (simple-value) a-s-val)
     (value ("(" comp-value val-compl ")") compl-val)

     ;;Complemento para valores
     (val-compl (calls) val-call) 
     (val-compl (bin-op comp-value) binop-val)

     ;; Valores simples
     (simple-value (identifier) id-val)
     (simple-value (number) int-val)
     (simple-value (text) str-val)
     (simple-value ("true") true-val)
     (simple-value ("false") false-val)
     (simple-value ("nil") nil-val)
     (simple-value ("["(separated-list comp-value ",")"]") arr-val)
     
     ;;Operacion Inorden
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
     (bin-op ("==") equal)
     (bin-op ("!=") not-equal)
     (bin-op ("and") and-op)
     (bin-op ("&&") and-op)
     (bin-op ("or") or-op)
     (bin-op ("||") or-op)

     ;;Rangos:
     (bin-op ("..") in-range)
     (bin-op ("...") ex-range)
     (bin-op ("step") st-range)
     
     ;;Operación asignación
     (assign-op ("+=") add-eq)
     (assign-op ("-=") diff-eq)
     (assign-op ("*=") mult-eq)
     (assign-op ("/=") div-eq)
     (assign-op ("**=") pow-eq)

     ;;Operación unitaria
     (un-op ("not") not-op)
     (un-op ("!") not-op)     
  )
)

;Construidos automáticamente:
(sllgen:make-define-datatypes lexical-spec grammar-spec)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexical-spec grammar-spec)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexical-spec grammar-spec))

;El Analizador Léxico (Scanner)

(define scan
  (sllgen:make-string-scanner lexical-spec grammar-spec))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm)) 
    (sllgen:make-stream-parser 
      lexical-spec
      grammar-spec)))

;*******************************************************************************************
;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body exp-batch?)
   (env environment?)
   ))

;apply-procedure: evalua el cuerpo de un procedimiento en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
               (eval-proc-batch body (extend-env ids args env))))))
  
;*******************************************************************************************
;evalua un programa
(define (eval-program pgm)
  (cases ruby-program pgm
    (a-program (c-decls a-batch)
               (elaborate-class-decls! c-decls)
               (eval-exp-batch a-batch (empty-env)))
    )
  )

;evalua un exp-batch,si el resultado de la ultima expresion del batch es un void o un ambiente, imprime un => nil
;de lo contrario imprimer su resultado.
;si la ultima expresion fue la definicion de una funcion, imprime el nombre de esta
(define (eval-exp-batch batch env)
  (cases exp-batch batch
    (a-batch (exp exps) (let ((result (eval-expressions (cons exp exps) env)))
                          (if (or (environment? result) (void? result))
                              (let ((is-func (is-last-exp-a-func (cons exp exps))))
                                (if (eqv? is-func #f)
                                    (begin (display "=> ") 'nil)
                                    is-func))
                              result)))))

;evalua el batch de un procedimiento
(define (eval-proc-batch batch env)
  (cases exp-batch batch
    (a-batch (exp exps)        
              (eval-expressions (cons exp exps) env))))

;evalua una lista de expresiones
;si una expresion retorna un ambiente, este se pasa a la evaluacion de la siguiente expresion de la lista
(define (eval-expressions exps env)
 (if (or (empty? (cdr exps)) (is-return-exp? (car exps)))                
     (eval-expression (car exps) env)     
     (let ((next-env (eval-expression (car exps) env)))
       (if (environment? next-env) (eval-expressions (cdr exps) next-env)
           (eval-expressions (cdr exps) env)))))

;evalua una expresion
(define (eval-expression exp env)
  (cases expression exp
    (a-simple-exp (simple-exp) (eval-simple-exp simple-exp env))

    (declare-exp (id ids) (extend-env (cons id ids) (map (lambda (x) "nil") (cons id ids)) env))
    
    (puts-exp (vals)
              (for-each (lambda (arg)
                          (if (check-apply-env env arg) (pretty-display (apply-env env arg))
                          (pretty-display arg)))
                        (map (lambda(x) (eval-comp-value x env)) vals)))

    (if-exp (if-comp if-batch elsif-comps elsif-batchs else-batch) (if (eqv? "true" (eval-comp-value if-comp env))
                                                                       (eval-exp-batch if-batch env)
                                                                       (if (or (empty? elsif-comps) (empty? elsif-batchs))
                                                                           (if (empty? else-batch)
                                                                               (void)
                                                                               (eval-exp-batch (car else-batch) env))
                                                                           (eval-expression (if-exp (car elsif-comps) (car elsif-batchs) (cdr elsif-comps) (cdr elsif-batchs) else-batch) env))))

    (unless-exp (comp-bool batch else-batch) (if (eqv? "false" (eval-comp-value comp-bool env))
                                                 (eval-exp-batch batch env)
                                                 (if (empty? else-batch)
                                                     (void)
                                                     (eval-exp-batch (car else-batch) env))))

    (function-exp (name ids batch) (a-recursive-env name ids batch env))
      
    (while-exp (comp-bool batch) (if (eqv? "true" (eval-comp-value comp-bool env))
                                     (let ((new-env(eval-exp-batch batch env)))
                                       (if (environment? new-env)
                                           (eval-expression (while-exp comp-bool batch) new-env)
                                           (eval-expression (while-exp comp-bool batch) env)))
                                     (void)))

    (until-exp (comp-bool batch) (if (eqv? "false" (eval-comp-value comp-bool env))
                                     (let ((new-env(eval-exp-batch batch env)))
                                       (if (environment? new-env)
                                           (eval-expression (until-exp comp-bool batch) new-env)
                                           (eval-expression (until-exp comp-bool batch) env)))
                                     (void)))   

    (return-exp (comp-value) (eval-comp-value comp-value env))

    (for-exp (id comp-value exp-batch)
             (for-each (lambda (x)
                         (eval-proc-batch exp-batch (extend-env (list id) (list x) env)))
                       (eval-comp-value comp-value env)))))

;evalua una expresion simple
(define (eval-simple-exp s-exp env)
  (cases simple-exp s-exp
    (val-exp (simple-value complement) (apply-complement simple-value complement env))))

;evalua los argumentos de los calls, retorna una lista con los argumentos de cada llamado evaluados
(define (eval-calls-args cls left-value env)
  (cases calls cls
    (some-calls (calls) (if (empty? calls)
                            '()
                            (cons (eval-call (car calls) left-value env)
                                  (eval-calls-args (some-calls (cdr calls)) left-value env))))))

;evalua un call teniendo en cuenta quien hace el llamdo
;si no tiene argumentos se retorna el valor evaluado de la expresion que hizo el llamado
;si los tiene podría tratarse de un llamado a un procedimiento o de un acceso a una lista
(define eval-calls
  (lambda (cls left-value env)
    (let ((args (eval-calls-args cls left-value env)))
      (if (and (list? args) (not (empty? args)) (not (empty? (car args)))) ;;(part? (car (car args))))
          (car args)
          (let ((left-value-evaluated (if (simple-value? left-value)
                                          (apply-env env (eval-simple-value left-value env))
                                          (eval-comp-value left-value env))))
            (if (empty? args)
                (if (symbol? left-value-evaluated)
                    (apply-env env left-value-evaluated)
                    left-value-evaluated)
                (if (procval? left-value-evaluated)
                    (apply-procedure left-value-evaluated (car args) env)
                    (let ((result (encontrar left-value-evaluated args)))
                      (if (and (eqv? (length result) 1))
                          (car result)
                          result)))))))))
              
;evalua un complemento
(define (apply-complement s-val compl env)
  (cases complement compl
    (assign (comp-value calls)
            (let ((id (apply-env-ref env (eval-simple-value s-val env))) (val (eval-calls calls comp-value env)))
              (begin     
                (setref! id val)
                val)))

    (assign-and (assign-op comp-value calls)
                (let ((left-val-ref (apply-env-ref env (eval-simple-value s-val env)))
                      (right-val (eval-calls calls comp-value env)))
                  (let ((result (apply-assign assign-op (deref left-val-ref) right-val)))
                    (begin
                      (setref! left-val-ref result)
                      result))))   
    (comp-calls (calls) (eval-calls calls s-val env))))                                        

;evalua un comp-value
(define (eval-comp-value c-value env)
  (cases comp-value c-value
    (a-value (value) (eval-value value env))
    (unop-value (un-op comp-value) (eval-un-op (eval-comp-value comp-value env)))))

;evalua la negacion
(define eval-un-op
  (lambda (val)
    (cond
      ((eqv? val "true") "false")
      ((eqv? val "false") "true")
      (else (eopl:error 'eval-un-op "Not a bool")))))

;evalua un value
(define (eval-value a-value env)
  (cases value a-value
    (a-s-val (simple-value) (if (symbol? (eval-simple-value simple-value env))
                               (apply-env env (eval-simple-value simple-value env))
                               (eval-simple-value simple-value env)))
    (compl-val (comp-value val-compl) (eval-val-compl comp-value val-compl env))))

;evalua un val-compl
(define (eval-val-compl comp-value v-compl env)
  (cases val-compl v-compl
    (val-call (calls) (eval-calls calls comp-value env))
    (binop-val (bin-op comp-v) (apply-op bin-op (list (eval-comp-value  comp-value env) (eval-comp-value comp-v env))))))

(define (get-class-id c-value env)
  (cases comp-value c-value
    (a-value (a-value) (cases value a-value
                         (a-s-val (simple-value) (eval-simple-value simple-value env))
                         (else (object->class-name (eval-comp-value c-value env)))))
    (else (object->class-name (eval-comp-value c-value env)))))

(define (eval-method-args args env)
  (cases arguments args
    (some-arguments (comp-values) (map (lambda (x) (eval-comp-value x env)) comp-values))
    (else (eopl:error 'Error "No args"))))

(define get-id
  (lambda (sv)
    (cases simple-value sv
      (id-val (val) val)
      (else #f))))

;evalua un call
(define (eval-call cl s-val env)
  (cases call cl
    (arguments-call (arguments) (eval-args arguments s-val env))
    (a-method-call (id arguments) (if (simple-value? s-val)
                                      (find-method-and-apply id
                                                             (object->class-name (apply-env env (get-id s-val)))
                                                             (apply-env env (get-id s-val))
                                                             (eval-method-args arguments env)) ;; is a variable
                                      (let ((c-decl (lookup-class (get-class-id s-val env))))
                                        (if (or (eqv? id 'new) (eqv? id 'New))
                                            (let ((obj (new-object (get-class-id s-val env))))
                                              (find-method-and-apply 'initialize (get-class-id s-val env) obj (eval-method-args arguments env)) obj)
                                            (eopl:error 'eval-call "Can't apply method to a class"))))) ;; is a class
                                        
    (else "")))

;evalua los argumentos de un call teniendo en cuenta quien hizo el llamado
;retorna error si algo que no es un procedimiento intenta hacer un llamado con ()
(define (eval-args args s-val env)
  (cases arguments args
    (some-arguments (comp-values) (if (simple-value? s-val)                                     
                                      (let ((proc (apply-env env (eval-simple-value s-val env))))                                        
                                        (if (procval? proc)
                                            (map (lambda (x) (eval-comp-value x env)) comp-values)
                                            (eopl:error 'Error "Can't apply args to ~s" proc)))                                      
                                      (let ((proc (eval-comp-value s-val env)))
                                        (if (procval? proc)
                                            (map (lambda (x) (eval-comp-value x env)) comp-values)
                                            (eopl:error 'Error "Can't apply args to ~s" proc)))))
                         
    (arr-arguments (comp-value comp-values) (map (lambda (x) (eval-comp-value x env)) (cons comp-value comp-values)))))

;evalua un simple-value
(define (eval-simple-value s-val env)
  (cases simple-value s-val
    (id-val  (identifier)  identifier)
    (int-val (number) number)
    (str-val (text) (string-trim text "\""))
    (true-val  () "true")
    (false-val () "false")
    (nil-val   () "nil")
    (arr-val   (comp-value) (map (lambda (x) (eval-comp-value x env)) comp-value))))

;evalua un bin-op
(define (apply-op op args)
  (cases bin-op op
     (add ()  (operacion args 'suma))
     (diff () (- (car args) (cadr args)))
     (mult () (operacion args 'mult))
     (div () (/ (car args) (cadr args)))
     (mod () (modulo (car args) (cadr args)))
     (pow ()(expt (car args) (cadr args)))
     (great () (if (> (car args) (cadr args)) "true" "false"))
     (great-eq () (if (>= (car args) (cadr args)) "true" "false"))
     (less () (if (< (car args) (cadr args)) "true" "false"))
     (less-eq () (if (<= (car args) (cadr args)) "true" "false"))
     (equal () (if (equal? (car args) (cadr args)) "true" "false"))
     (not-equal () (if (not(equal? (car args) (cadr args))) "true" "false"))
     (and-op () (if (and (eval-bool (car args)) (eval-bool (cadr args))) "true" "false"))
     (or-op () (if (or (eval-bool (car args)) (eval-bool (cadr args))) "true" "false"))
     (in-range () (rango (car args) (cadr args) 'in))
     (ex-range () (rango (car args) (cadr args) 'ex))
     (st-range () (steps (car args) (cadr args)))))

;traduce de un bool de ruby a uno de racket
(define eval-bool
  (lambda (val)
    (if (eqv? val "true") #t #f)))

;evalua un assign-op
(define (apply-assign op arg1 arg2)
  (cases assign-op op
    (add-eq () (+ arg1 arg2))
    (diff-eq () (- arg1 arg2))
    (mult-eq () (* arg1 arg2))
    (div-eq () (/ arg1 arg2))
    (pow-eq () (expt arg1 arg2))))

#|.......................................FUNCIONES AUXILIARES............................................|#
;evualua si la ultima expresion de un batch es una definicion de una funcion, de serlo retorna el nombre de ella
(define is-last-exp-a-func
  (lambda (exps)   
    (if (empty? exps)
        #f
        (if (empty? (cdr exps))
            (if (is-function-exp? (car exps))
                (get-function-name (car exps))
                #f)             
            (is-last-exp-a-func (cdr exps))))))

;retorna el nombre de una funcion a partir de un function-exp
(define (get-function-name exp)
  (cases expression exp
    (function-exp (name ids batch) name)
    (else "")))

;evalua si una expresion es de tipo return-exp
(define (is-return-exp? exp)
  (cases expression exp
    (return-exp (comp-value) #t)
    (else #f))) 

;evalua si una expresion es de tipo function-exp
(define (is-function-exp? exp)
  (cases expression exp
    (function-exp (name ids batch) #t)
    (else #f)))
    
#|Función que opera seguún el tipo de dato que contenga la lista|#
(define (operacion lista sys)
  (cond
    [(equal? lista '()) "Error"]
    [(and (number?(car lista))(list? (cadr lista)) (equal? sys 'suma))"Error"]
    [(and (list? (car lista)) (number? (cadr lista)) (equal? sys 'suma))"Error"]
    [(and (number?(car lista))(string? (cadr lista)) (equal? sys 'suma))"Error"]
    [(and (string? (car lista)) (number? (cadr lista)) (equal? sys 'suma))"Error"]   
    #|Si mi lista de args son puras cadenas entonces sé que su suma equivale a concatenar ambos|#
    [(and (string? (car lista)) (string? (cadr lista)) (eq? sys 'suma))

     ;---AQUÍ LO QUE HAGO ES LIMPIAR MI CADENA DE ESTO: #\"
     (list->string(eliminar(string->list
     (string-append (car lista) (cadr lista)))))]
    ;---------------------------------------------------
#|Si el simbolo es 'mult tengo que verificar que almenos uno de los elemenos sea un número para saber
cuantas veces repetir mi cadena|#
    [(or
      (and (string? (car lista)) (number? (cadr lista)) (eq? sys 'mult))
      (and (number? (car lista)) (string? (cadr lista)) (eq? sys 'mult)))

     ;---AQUÍ LO QUE HAGO ES LIMPIAR MI CADENA DE ESTO: #\"
      (list->string(eliminar(string->list(mul (car lista) (cadr lista)))))]
    ;---------------------------------------------------
    
    #|Si no se cumple que almenos uno sea cadena entonces hay que verificar si ambos son números. Así,
ya se trate de una suma o una multiplicación, los puedo operar de manera común y corriente|#
    [(and (number? (car lista)) (number? (cadr lista)))
             (if (eq? sys 'suma) (+ (car lista) (cadr lista))
                                 (* (car lista) (cadr lista)))]
    #|Si no cumple ninguna, entonces se trata de un arreglo y debo usar funciones auxiliares|#
    [else (if (eq? sys 'suma)
              (append (car lista) (cadr lista))             
              (duplicar (car lista) (cadr lista)))]))

#|Duplica el contenido de una lista n veces|#
(define (duplicar lista n)
  (cond
    [(eq? '() lista) '()]
    [(or (list? n)(string? n)) "Error"]
    [(= n 0)'()]
    [else (append lista (duplicar lista (- n 1)))]))

#|Suma dos arreglos teniendo en cuenta la manera en que son recibidos|#
(define (operar lista1 lista2)
  (cond
    [(and (equal? lista1 '()) (equal? lista2 '())) '()]
    [(append (list (+ (car lista1)
                      (car lista2)))
             (operar (cdr lista1)
                     (cdr lista2)))]))

#|MULTIPLICAR CADENAS|#
#|Esta función crea una sola cadena que realmente es una cadena repetida n veces|#
(define (mul cadena n)
  (cond
    [(and (number? cadena) (string? n)) (mul n cadena)]
    [(= n 0) ""]
    [(string-append cadena (mul cadena (- n 1)))]))

#|ELIMINAR DIAGONALES DEL TEXTO|#
#|dado a que las cadenas son acompañadas por un #\" entonces hago esta función para eliminarlas y devolver una
cadena más limpia|#
(define (eliminar lista)
  (cond
    [(eq? '() lista)'()]
    [(eq? (car lista) #\")(append (eliminar (cdr lista)))]
    [else (append (list(car lista)) (eliminar (cdr lista)))]))

#|Encontrar:
  Debo partir de la siguiente premisa para entender lo que debo hacer:
  Hay dos posibilidades cuando tratamos de obtener un valor de un arreglo.
a)El rango siempre será una lista de longitud 2. Dado a que su naturaleza es tener un inicio y un final.
;-------------------POSIBILIDADES 1-----------------
(define lista1 (list (list (list 1 2) 3) 4));<----arreglo(longitud 2)=[[[1,2],3],4]
(define lista4 (list (list 0) (list 0) (list 0)))  ;<----lista de rangos de longitud 1)= arreglo[0][0]
;-------------------POSIBILIDADES 2-----------------
(define lista2 (list(list 1 2 3 4)));<----arreglo (longitud 1)=[1,2,3,4]
(define lista3 (list (list 0 3)));<-------rango   (longitud 2)= arreglo[0,3]
|#
#|Función encontrar, encuentra un dato dentro de un arreglo.|#
(define (encontrar lis-vals lis-pos)
  (cond
#|Si alguna no es de longitud uno, lo que equivale a un arreglo de posibles arreglos y una posición compuesta
(arreglo[in][end]), entonces usamos la función general con ambas listas|#
    [(not (or (= 1 (length lis-vals)) (= 1(length lis-pos)))) (comp-range-array lis-vals lis-pos)]
    
    #|Si la condición anterior no se cumple, estaremos hablando de que ambas son listas simples o sencillas
(un arreglo de sólo números un llamado a posición que va de in a end sin más). Por esto, llamaremos a
simple-range-array que recibe un inicio, un final, una lista y un ac (ac será sólo con propósitos comparativos).
Por lo tanto, invocaremos tal función de la siguiente manera:
inicio = primer valor de la lista lis-pos (array[0,2]->'('(0 2)))->0)
final  = segundo valor de la lista lis-pos(array[0,2]->'('(0 2)))->2)(Si estoy haciendo array[0] entonces end=in)
lista  = (car lis-vals)-->(car(list lis-vals))
ac     = (car lis-vals)-->(car(list lis-vals))|#
    [else (simple-range-array (caar lis-pos) (if (= 1 (length(car lis-pos)))
                                          (caar lis-pos) (cadar lis-pos)) lis-vals lis-vals)]))

#|Arreglo simple, posición simple|#
(define (simple-range-array in end lis ac)
  (cond
    #|Tenemos que verificar que in no sea negativo porque tal posición no existe y además debemos verificar que
end no sea mayor que la máxima posición de la lista (dado a que la lista la estaremos modificando por motivos de
recursión, usaremos ac)|#
    [(or (< in 0)  (> end (- (length ac) 1))) "Error"]
    #|Si la lista está vacía devolvemos vación ya que el propósito de esta función es retornar una lista|#
    [(empty? lis) '()]
    #|Nuestro punto de parada es cuando nuestro inicio llegue a su final, entonces devolveremos lo que contenga
      el dato en esa posición de la lista ingresada. Si lo que devuelve es una lista, la entregamos tal como está
      si lo que devuelve es un dato diferente a lista, devolvemos el dato dentro de una lista.|#
    [(= in end)  (if (list? (list-ref lis in)) (list-ref lis in) (list (list-ref lis in)))]
    #|Mientras que el punto de parada no se cumpla, concatenaremos el dato que se encuentra en la posición in
      con la recurisón de simple-range-array pero ahora in se aumentará en uno y su end,lis y ac serán los mismos.|#
    [else (append (list (list-ref lis in)) (simple-range-array (+ in 1) end lis ac))]))


#|Cuando no se trate de un arreglo simple y por lo tanto de una llamado a posicionamiento simple, acudiremos a
la función encon que recibe dos listas|#
(define (encon lista1 lista2)
  (cond
#|Punto de parada: cuando la lista a la cual le estamos buscando un número, termine siendo el mismo entonces
devolverlo.|#
    [(number? lista1) lista1]
#|Mientras ningun punto de parada se cumpla seguiremos haciendo llamado a encontrar (para que se encargue de decidir
si hemos llegado a un punto donde las lista1 es simple (arreglo sencillo) o seguimos teniendo un arreglo compuesto).
pero la lista donde buscaremos el dato será el producto de hacer list-ref con la lista1 intacta y como posición tendremos
el primer dato, del primer dato de lista2 -arreglo[0][1]-->'('(0)'(1))--> 0-. Como lista2 pasaremos el resto de esta.|#
    [else (encontrar (list-ref lista1 (caar lista2)) (cdr lista2))]))

#|Función bool me regresa una lista de dos booleanos las cuales me servirán de indicador para saber si mi llamado
es compuesto o no.|#
(define (bool lista)
  (map (lambda (x) (= 1 (length x)))lista))


#|----------------------Funcion evalua arreglos compuestos-------------------------------------------------|#
(define (comp-range-array lista1 lista2)
  (cond
#|Si bool devuelve una lista con dos datos de tipo #true entonces estaremos hablando de una lista de posiciones compuesta
 con llamado a dato en una sola posición (Ej: array[0][0]). Entonces,haremos llamado a encon con ambas listas|#
    [(and (equal? #t (car (bool lista2)))
          (equal? #t (cadr (bool lista2)))) (encon lista1 lista2)]
    
 #|Si no devuelve una lista de sólo true's quiere decir que tenemos un arreglo compuesto y llamado de posición compuesto
que desea sacar una fragmento de un arreglo contenido sobre otro.
Ej: array = [[1,2,3],4,5]; puts(array[0][0,2]);
Por lo tanto llamaremos a la función principal encontrar para que se encargue de evaluar de nuevo todo y sacar
lo necesario a través de las diferentes funciones auxiliares|#
    
    [else (encontrar (encon lista1 lista2) (cdr lista2))]))

; eval-simple-value s-val env (cases simple-value s-val (id-val ...) (num-val ...) (true-val ...))
;   evalúa un valor simple, comprende los casos desde id-val hasta arr-val
;   para el caso de id-val se debe hacer apply-env

#|Función que dependiendo de el simbolo, realiza una lista excluyente o incluyente en un rango origen-destino|#
(define (rango origen destino sym)
  (cond
    #|Me aseguro que origen y destino sean números para no cometer errores de operación|#
    [(not(or(number? origen) (number? destino))) "Error"]
    [(equal? sym 'in) (inclu origen destino)]
    [(equal? sym 'ex) (exclu origen destino)]
    #|Si no se digita el sym permitido, debo mostrar error|#
    [else "Error"]))

#|La función inclu realiza una lista que va desde origen hasta destino siendo el punto de parada cuando origen
sea igual a destino|#
(define (inclu origen destino)
  (cond
    [(or (list? origen) (list? destino))"Error"]
    [(= origen destino) (list origen)]
    [(< origen destino) (append (list origen) (inclu (+ origen 1) destino))]
    [(> origen destino) (reverse(inclu destino origen))]))

#|La función exclu realiza una lista que va desde origen hasta destino siendo el punto de parada cuando origen
sea igual a destino pero con la diferencia que el punto de para devuelve vacio.|#
(define (exclu origen destino)
  (cond
    [(or (list? origen) (list? destino))"Error"]
    [(= origen destino) empty]
    [(< origen destino) (append (list origen) (exclu(+ origen 1) destino))]
    [(> origen destino) (append (list origen) (exclu(- origen 1) destino))]))

#|Para explicar pasos, tendremos de ejemplo lo siguiente:
lista       = '(1 2 3 4 5)
paso        = 2
lis-compare = lista|#
(define (pasos lista paso lis-compare)
  (cond
    [(empty? lista) '()];<----Si la lista está vacía, devolver vacío (Dado a que la función retorna una lista).
    
    [(or(and (< (car lis-compare) (last lis-compare))                   ;|  Verificar que sea posible hacer paso con la
             (> 0 paso))(and (> (car lis-compare) (last lis-compare))   ;|->lista dada. Dado a que la lista se verá
                             (< 0 paso)))(eopl:error 'Step "bad step")] ;|  afectada por recursión usamos lis-compare
    
    [else (append (list(car lista)) ;->Concatenar el primero de la lista (1)...
                  (pasos (if (positive? paso)                                              ;|...con el llamado recursivo
                             (if(> paso (length lista)) '() (list-tail lista paso))        ;|->pero ahora lista será
                      (if(> (* -1 paso) (length lista)) '() (list-tail lista (* -1 paso))));|'(3 4 5) gracias a list-tail
                         paso                                                              ;| paso será el mismo
                         lis-compare))]))                                                  ;|y lis-compare será la misma.

(define (steps lista paso)
  (pasos lista paso lista))

;*******************************************************************************************
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

;*******************************************************************************************
;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec  vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
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

; |Ambiente recursivo para un solo procedimiento

(define (a-recursive-env a-proc-name ids body env)
  (let ((vec (make-vector 1)))
    (let ((env (extended-env-record (list a-proc-name) vec env)))
          (vector-set! vec 0 (closure ids body env))
          env)
    )
  )

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)  
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'Error "undefined local variable or method ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;valida un simbolo en el ambiente
;función que busca un símbolo en un ambiente
(define check-apply-env
  (lambda (env sym)
    (check-apply-env-ref env sym)))

(define check-apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        #f)
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 #t
                                 (check-apply-env-ref env sym)))))))

;*******************************************************************************************
;*******************************************************************************************
;Ambiente inicial

(define (init-env) (empty-env))
;*******************************************************************************************
;*******************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

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

;*******************************************************************************************
;*******************************************************************************************

;;Función que retorna una lista dado un inicio, un final, y un incremento 
(define iota-range
  (lambda (start end step)
    (cond [(or
            (and (< start end) (> 0 step))
            (and (> start end) (< 0 step)))
           (eopl:error 'Step "bad step")]
          [else
           (let loop ((next start))
             (if (= 0 (abs (- next end)))
                 (list next)
                 (cons next (loop (+ next step)))))]
          )
    ))

;*******************************************************************************************
;*******************************************************************************************

;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

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

;*******************************************************************************************
;*******************************************************************************************

(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (and (list? class-name) (empty? class-name));;(eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-exp-batch body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (map (lambda (x) (string->symbol (string-append "@" (symbol->string x)))) (part->field-ids (car parts)))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
      
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) #f)
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

(interpretador)

