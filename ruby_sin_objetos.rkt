#lang racket
(require eopl)
(require racket/string); string-trim
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
  '( ;;Representa un programa de ruby     
     (ruby-program ("ruby" exp-batch "end") a-program)
     ;; Parte 2: Ruby con objetos
     ;; cambiar a: (ruby-program ("ruby" (arbno class-decl) exp-batch "end") a-program)
     
     ;;Exp-batch: Representa una cerradura de expresiones
     (exp-batch (expression (arbno expression)) a-batch)

     ;;Expresión:
     (expression (simple-exp) a-simple-exp)
     ;Declare-exp: al menos uno o más identificadores (deben inicializarse en 'nil)
     (expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
     ;Puts-exp: al menos un valor compuesto para imprimir
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

     ;;Expresión simple
     (simple-exp (simple-value complement ";") val-exp)
     
     ;;Complemento
     (complement ("=" comp-value calls) assign)
     (complement (assign-op comp-value calls) assign-and)
     (complement (calls) comp-calls)

     ;;Calls
     ;; 0 o muchas llamadas
     (calls ((arbno call)) some-calls)

     ;;Call
     (call (arguments) arguments-call)
     ;; (call ("." identifier arguments) a-method-call) ;; Parte 2: Ruby con Objetos

     ;;Argumentos
     ;; llamar una función puede tener 0 argumentos o muchos
     (arguments ("(" (separated-list comp-value ",") ")") some-arguments)
     ;; almenos 1 argumento para llamar acceder a un elemento en un arreglo
     ;; máximo 2, ejemplo: a=[1,2,3]; a[1] #output 2; a[1,2] #output [2,3];
     ;;                    a[1,2,3] #output Error
     (arguments ("[" comp-value (arbno "," comp-value) "]") arr-arguments)

     ;;Valores compuestos
     (comp-value (value) a-value)
     (comp-value (un-op comp-value) unop-value)
     
     (value (simple-value) a-s-val)
     (value ("(" comp-value val-compl ")") compl-val)

     ;;Complemento para valores
     ;; llamadas a un valor:
     ;; Ejemplo: sirve para ("hola"+(mundo())) donde mundo() retorna "mundo"
     (val-compl (calls) val-call) 
     ;; operacion inorden con otro valor
     (val-compl (bin-op comp-value) binop-val)

     ;; Valores simples
     (simple-value (identifier) id-val)
     (simple-value (number) int-val)
     (simple-value (text) str-val) ;; recordar hacer string-trim cuando se evalue
     (simple-value ("true") true-val)
     (simple-value ("false") false-val)
     (simple-value ("nil") nil-val)
     ;; arreglo con 0 o muchos valores
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
     ;; Solo admite 2 argumentos, no se puede operar más de 1 vez
     ;;Inclusivo: va hasta el limite superior
     (bin-op ("..") in-range)
     ;;Exclusivo: va hasta un step antes del limite superior
     (bin-op ("...") ex-range)
     ;; Ejemplo: (1..5) => (1 2 3 4 5)
     ;; Ejemplo: (1...5) => (1 2 3 4)
     ;; Ejemplo: ((1..5) .. 6) => Error
     (bin-op ("step") st-range)
     ;; Ejemplo: ((1..5) step 2) => (1 3 5)
     ;; Ejemplo: ((1..5) step -1) => Error
     ;; Ejemplo: ((-1..-5) step -2) => (-1 -3 -5)
     ;; Ejemplo: ((1..-5) step 2) => Error
     
     ;;Operación asignación
     (assign-op ("+=") add-eq)
     (assign-op ("-=") diff-eq)
     (assign-op ("*=") mult-eq)
     (assign-op ("/=") div-eq)
     (assign-op ("**=") pow-eq)

     ;;Operación unitaria
     (un-op ("not") not-op)
     (un-op ("!") not-op)

     ;;##############################################
     ;; Parte 2: Ruby con objetos
     ;(class-decl ("class" identifier
     ;                     (arbno "<" identifier)
     ;                     "attr" (separated-list ":" identifier ",") ";"
     ;                     (arbno method-decl) "end") a-class-decl)

     ;(method-decl ("def" identifier "(" (separated-list identifier ",") ")"
     ;             exp-batch                  
     ;             "end") a-method-decl)

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

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
               ; |Evaluar el cuerpo de una función
;;quitar '
               '(eval-exp-batch body (extend-env ids args env))
               )
      )
    )
  )
;*******************************************************************************************

(define (eval-program pgm)
  (cases ruby-program pgm
    (a-program (a-batch) (eval-exp-batch a-batch (empty-env)))
    )
  )

(define (eval-exp-batch batch env)
  (cases exp-batch batch
    (a-batch (exp exps) (eval-expressions (cons exp exps) env))
    )
  )

(define (eval-expressions exps env)
 (if (empty? (cdr exps)) (let ((result(eval-expression (car exps) env)))
                           (if (or (environment? result) (void? result)) (begin (display "=> ") 'nil) result))
     (let ((next-env (eval-expression (car exps) env)))
       (if (environment? next-env) (eval-expressions (cdr exps) next-env)
           (eval-expressions (cdr exps) env)))))
  #|(let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        (if (void? acc) (begin (display "=> ") 'nil) acc)
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))|#

(define (eval-expression exp env)
  (cases expression exp
    (a-simple-exp (simple-exp) (eval-simple-exp simple-exp env))
    (declare-exp (id ids) (extend-env (cons id ids) (map (lambda (x) 'nil) (cons id ids)) env))
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

    (for-exp (id comp-value exp-batch)
              (for-each (lambda (x)
                          (eval-exp-batch exp-batch (extend-env (list id) (list x) env)))
                          (eval-comp-value comp-value env)))
    ;function-exp
    (return-exp (comp-value) (eval-comp-value comp-value env))
    (else "TO DO")))



(define (asignar exps ids args env)
  (if (equal? exps '())'() (eval-expressions exps (extend-env ids args env))))

(define (eval-simple-exp s-exp env)
  (cases simple-exp s-exp
    (val-exp (simple-value complement) (apply-complement simple-value complement env))))

(define eval-complement-ass
  (lambda (comp-value calls env)
    (cond
      ((empty?(is-comp-calls-empty calls env)) (eval-comp-value comp-value env))
      (else "TODO-handle proc calls"))))


(define (is-comp-calls-empty cls env)
  (cases calls cls
    (some-calls (cls) cls) ;<-- Aquí iba (empty? cls) porque ese bool lo usaba eval-complement-ass
                           ;Así que para no afectar eso, lo pregunté directamente en el eval-complement-ass
    (else "")))


(define (apply-complement s-val compl env)
  (cases complement compl
    (assign (comp-value calls)
            (let ((id (apply-env-ref env (eval-simple-value s-val env))) (val (eval-complement-ass comp-value calls env)))
              (begin
                (setref! id val)
                ;val<---Ya no, de esta manera si hago declare a; a = 3; end, eso no devuelve nada
                )))                     
    
;-----------------------------------------------------------------------------------------------------------------
#|Lo que hacemos aquí es:
  1) args = lista(lo-que-vale-mi-id  lo-que-vale-mi-comp-value)
  2) ids  = lista(id)
  3) evaluar el resto de expresiones con base en un nuevo ambiente extendido que contiene ids args y el env-0|#
    (assign-and (assign-op comp-value calls)
                (let((arg (apply-assign assign-op(append (list(apply-env env (eval-simple-value s-val env)))
                                                         (list(eval-comp-value comp-value env)))))
                     (id (apply-env-ref env (eval-simple-value s-val env))))
                  (begin
                    (setref! id arg)
                    arg)))
;-----------------------------------------------------------------------------------------------------------------
    (comp-calls (calls)
                ;(apply-env env(eval-simple-value s-val env))
                (eopl:error 'eval-un-op "No"))));--->La idea es que no se pueda hacer un llamado a secas
                          ;     ("a;") si no que sea necesario el return
;----------------------------------------------------------------------------------------------------------------------

(define (eval-comp-value c-value env)
  (cases comp-value c-value
    (a-value (value) (eval-value value env))
    (unop-value (un-op comp-value) (eval-un-op (eval-comp-value comp-value env)))))

(define eval-un-op
  (lambda (val)
    (cond
      ((eqv? val "true") "false")
      ((eqv? val "false") "true")
      (else (eopl:error 'eval-un-op "Not a bool")))))      

; eval-value a-value env => (cases value a-value (simple-val ...) (compl-val ...))
;   evalúa un valor, tiene 2 casos:
;     1) un valor simple que se evaluaría con eval-simple-value
;     2) un valor con complemento, se llama eval-val-compl con c-val evaluado y
;        a-val-compl (c-val es un comp-value, a-val-compl es un val-compl)
(define (eval-value a-value env)
  (cases value a-value
    (a-s-val (simple-value)(if (symbol? (eval-simple-value simple-value env))
                               (apply-env env (eval-simple-value simple-value env))
                               (eval-simple-value simple-value env)))
    (compl-val (comp-value val-compl) (eval-val-compl (eval-comp-value comp-value env) val-compl env))))

; eval-val-compl a-val a-val-compl env => (cases val-compl a-val-compl (val-call ...) (binop-val ...))
;  evalúa un complemento sobre un c-val, tiene 2 casos:
;    1) val-call(some-calls) entonces se tienen que aplicar los argumentos
;    2) binop-val(binop c-val) entonces se aplica una bin-op entre a-val y la evaluacion
;       de c-val (c-val es un comp-value)



(define (eval-val-compl a-val a-val-compl env)
  (cases val-compl a-val-compl
    (val-call (calls) (encontrar ;<--- encuentra un dato de un arreglo con otra lista que suponemos es de posiciones
                       a-val ;<--- lista del arreglo
                       (map (lambda (x) (eval-call x env))(is-comp-calls-empty calls env));<---lista de posiciones
                       ))
              
    
    (binop-val (bin-op comp-value) (apply-op bin-op (reverse(list (eval-comp-value comp-value env) a-val))))))

#|eval-call recibe una call en vez de una lista calls, un call puede ser arguments-call|#
(define (eval-call cl env)
  (cases call cl
    (arguments-call (arguments) (eval-args arguments env));<--Si es así, evaluo los argumentos en el ambiente ingresado
    (else "")))

#|Eval-args evalua un argumento en un ambiente|#
(define (eval-args arg env)
  (cases arguments arg
    #|Si es de tipo some-arguments simplemente evalua el comp-value en eval-comp-value|#
    (some-arguments (comp-value) (map (lambda (x)(eval-comp-value x env))
                                                       comp-value))
    #|Si es de tipo arreglo el argumento, uso eval-comp-value para evaluar el comp-value (no sé que hacer con
el comp-values porque ni siquiera sé que debería contener)|#
    (arr-arguments (comp-value comp-values) (append(list(eval-comp-value comp-value env))
                                                  (map (lambda (x)(eval-comp-value x env))
                                                       comp-values)
                                                  ))))
#|Encontrar:
  Debo partir de la siguiente premisa para entender lo que debo hacer:
  Hay dos posibilidades cuando tratamos de obtener un valor de un arreglo.

a)El rango siempre será una lista de longitud 2. Dado a que su naturaleza es tener un inicio y un final.
b)El arreglo es una lista de máxima  longitud 2. Será de longitud uno, cuand el arreglo que manejamos es un arreglo
que no contiene ningún otro arreglo. Y será de longitud dos, cuando éste posea arreglos dentro de sí.

;-------------------POSIBILIDADES 1-----------------
(define lista1 (list (list (list 1 2) 3) 4));<----arreglo(longitud 2)=[[[1,2],3],4]
(define lista4 (list (list 0) (list 0 1)))  ;<----rango  (longitud 2)=arreglo[0][0,1]

;-------------------POSIBILIDADES 2-----------------
(define lista2 (list(list 1 2 3 4)));<----arreglo (longitud 1)=[1,2,3,4]
(define lista3 (list (list 0 3)));<-------rango   (longitud 2)=arreglo[0,3]
|#

#|Función encontrar, encuentra un dato dentro de un arreglo.|#
(define (encontrar lis-vals lis-pos)
  (cond
#|Sería erroneo tratar de hacer algo así: arreglo[0][0] sabiendo que el arreglo es [1,2,3,4]. Así que
para verificar que no se cometa ese error; miramos con andmap que el arreglo sea uno simple (su contenido sea
sólo números) y que la lista de posiciones no sea una lista de listas (lo que equivale a arreglo[in][end])|#
    [(and (andmap number? lis-vals) (andmap list? lis-pos)) "Error 1"]

#|Si ambas no son de longitud uno, lo que equivale a un arreglo de posibles arreglos y una posición compuesta
(arreglo[in][end]), entonces usamos la función general con ambas listas|#
    [(not (and (= 1 (length lis-vals)) (= 1(length lis-pos)))) (comp-range-array lis-vals lis-pos)]
    
    #|Si la condición anterior se cumple, estaremos hablando de que ambas son listas simples o sencillas
(un arreglo de sólo números un llamado a posición que va de in a end sin más). Por esto, llamaremos a
simple-range-array que recibe un inicio, un final, una lista y un ac que será sólo con propósitos comparativos.

Por lo tanto, invocaremos tal función de la siguiente manera:
inicio = primer valor de la lista lis-pos (array[0][2]->'('(0 2)))->0)
final  = segundo valor de la lista lis-pos(array[0][2]->'('(0 2)))->2)
lista  = (car lis-vals)-->(car(list lis-vals))
ac     = (car lis-vals)-->(car(list lis-vals))|#
    [else (simple-range-array (caar lis-pos) (if (= 1 (length(car lis-pos)))
                                          (caar lis-pos) (cadar lis-pos)) (car lis-vals) (car lis-vals))]))

#|Arreglo simple, posición simple|#
(define (simple-range-array in end lis ac)
  (cond
    #|Tenemos que verificar que in no sea negativo porque tal posición no existe y además debemos verificar que
end no sea mayor que la máxima posición de la lista (dado a que la lista la estaremos modificando por motivos de
recursión, usaremos ac)|#
    [(or (< in 0)  (> end (- (length ac) 1))) "Error"]
    #|Si la lista está vacía devolvemos vación ya que el propósito de esta función es retornar una lista|#
    [(empty? lis) '()]
    #|Nuestro punto de parada es cuando nuestro inicio llegue a su final, entonces devolveremos una lista
que contenga el dato en esa posición de la lista ingresada.|#
    [(= in end) (list (list-ref lis in))]
    #|Mientras que el punto de parada no se cumpla, concatenaremos el dato que se encuentra en la posición in
con la recurisón de simple-range-array pero ahora in se aumentará en uno y su end,lis y ac serán los mismos.|#
    [else (append (list (list-ref lis in)) (simple-range-array (+ in 1) end lis ac))]))


#|Cuando no se trate de un arreglo simple y por lo tanto de una llamado a posicionamiento simple, acudiremos a
la función encon que recibe dos listas|#
(define (encon lista1 lista2)
  (cond
#|Primer punto de parada, cuando la lista a la cual le estamos buscando un número, termine siendo el mismo entonces
devolverlo.|#
    [(number? lista1) lista1]
#|El otro punto de parada es cuando nuestra lista1 que no es un arreglo simple, se convierta en esta, así la devolveremos
para evaluarla con simple-range-array|#
    [(andmap number? lista1) lista1]
#|Mientras ningun punto de parada se cumpla seguiremos haciendo recursión con econ
pero la lista donde buscaremos el dato será el producto de hacer list-ref con la lista1 intacta y como posición tendremos
el primer dato, del primer dato de lista2 -arreglo[0][1]-->'('(0)'(1))--> 0-. Como lista2 pasaremos el resto de esta.|#
    [else (encon (list-ref lista1 (caar lista2)) (cdr lista2))]))

#|Función bo me dice si una lista posee listas con longitud 1, lo que equivale a devolver una lista de longitud dos
(ya que la lista es de posiciones y esta siempre será de longitud 2) que contiene dos booleanos|#
(define (bo lista)
  (map (lambda (x) (= 1 (length x)))lista))


#|----------------------Funcion evalua arreglos compuestos-------------------------------------------------|#
(define (comp-range-array lista1 lista2)
  (cond
#|Si bo devuelve una lista con dos datos de tipo #true entonces estaremos hablando de una lista de posiciones compuesta
 con llamado a dato en una sola posición. Entonces,haremos llamado a encon con ambas listas|#
    [(and (equal? #t (car (bo lista2)))
          (equal? #t (cadr (bo lista2)))) (encon lista1 lista2)]
    
 #|Si no devuelve una lista de sólo true's quiere decir que tenemos un arreglo compuesto y llamado de posición compuesto
que desea sacar una fragmento de un arreglo contenido sobre otro.
Ej: array = [[1,2,3],4,5]; puts(array[0][0,2]);
Por lo tanto llamaremos a la función principal encontrar para que se encargue de evaluar de nuevo todo y sacar
lo necesario a través de las diferentes funciones auxiliares|#
    
    [else (encontrar (list(encon lista1 lista2)) (cdr lista2))]))

; eval-simple-value s-val env (cases simple-value s-val (id-val ...) (num-val ...) (true-val ...))
;   evalúa un valor simple, comprende los casos desde id-val hasta arr-val
;   para el caso de id-val se debe hacer apply-env

(define (eval-simple-value s-val env)
  (cases simple-value s-val
    (id-val  (identifier)  identifier);;(apply-env env identifier))
    (int-val (number) number)
    (str-val (text) text)
    (true-val  () "true")
    (false-val () "false")
    (nil-val   () "nil")
    (arr-val   (comp-value) (map (lambda (x) (eval-comp-value x env)) comp-value))))

(define (apply-op op args)
  (cases bin-op op
     (add ()  (operacion args 'suma))
     (diff () (- (car args) (cadr args)))
     (mult () (operacion args 'mult))
     (div () (/ (car args) (cadr args)))
     (mod () (modulo (car args) (cadr args)))
     (pow ()(potencia (car args) (cadr args)))
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

(define eval-bool
  (lambda (val)
    (if (eqv? val "true") #t #f)))

(define (apply-assign op args)
  (cases assign-op op
    (add-eq   ()   (+(car args) (cadr args)))
     (diff-eq ()   (-(car args) (cadr args)))
     (mult-eq ()   (*(car args) (cadr args)))
     (div-eq  ()   (/(car args) (cadr args)))
     (pow-eq  ()   (potencia(car args) (cadr args)))))

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
    [(= origen destino) (list origen)]
    [(< origen destino) (append (list origen) (inclu (+ origen 1) destino))]
    [(> origen destino) (reverse(inclu destino origen))]))

#|La función exclu realiza una lista que va desde origen hasta destino siendo el punto de parada cuando origen
sea igual a destino pero con la diferencia que el punto de para devuelve vacio.|#
(define (exclu origen destino)
  (cond
    [(= origen destino) empty]
    [(< origen destino) (append (list origen) (exclu(+ origen 1) destino))]
    [(> origen destino) (append (list origen) (exclu(- origen 1) destino))]))

(define (pasos lista paso lis-compare)
  (cond
    [(empty? lista) '()]
    [(andmap false? (map (lambda (dato) (= dato paso)) lis-compare)) "Error"]
    [else (append (list(car lista))
                  (pasos (if (positive? paso)
                             (if(> paso (length lista)) '() (list-tail lista paso))
                             (if(> (* -1 paso) (length lista)) '() (list-tail lista (* -1 paso))))
                         paso
                         lis-compare))]))

(define (steps lista paso)
  (pasos lista paso lista))
#|La función potencia elevan una base a la n potencia|#
(define (potencia base n)
  (cond
    [(= n 1) base]
    [(= n 0) 1]
    [else (* base (potencia base (- n 1)))]))

#|Función que opera seguún el tipo de dato que contenga la lista|#
(define (operacion lista sys)
  (cond
    [(equal? lista '()) "Error"]
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
    [else (if (eq? sys 'suma)(operar (car lista) (cadr lista))
             (duplicar (car lista) (cadr lista)))
     ]))

#|Duplica el contenido de una lista n veces|#
(define (duplicar lista n)
  (cond
    [(eq? '() lista) '()]
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
      (empty-env-record ()
                        (eopl:error 'Error "undefined local variable or method ~s" sym))
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
;;;Rangos
(define-datatype range range?
  (inclusive (start number?) (end number?) (step number?))
  (exclusive (start number?) (end number?) (step number?))
  )

(define (eval-range a-range)
  (cases range a-range
    (inclusive (start end step) (iota-range start end step))
    (exclusive (start end step) (iota-range start (- end 1) step))
    )
  )

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

; #Ejemplos:
; > (eval-range (inclusive 1 10 1))
; (1 2 3 4 5 6 7 8 9 10)
; > (eval-range (exclusive 1 10 1))
; (1 2 3 4 5 6 7 8 9)
; > (eval-range (inclusive 1 -10 1))
; . . Step: bad step
; > (eval-range (inclusive -1 10 -1))
; . . Step: bad step


;*******************************************************************************************
;*******************************************************************************************
(interpretador)
