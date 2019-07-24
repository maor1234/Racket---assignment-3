
#lang pl

(define-type BIT = (U 0 1))
   (define-type Bit-List = (Listof BIT))


   (define-type
    RegE
    [Reg Bit-List]
    [And RegE RegE]
    [Or RegE RegE]
    [Shl RegE]
    [Id Symbol]
    [With Symbol RegE RegE]
    [Bool Boolean]
    [Geq RegE RegE]
    [Maj RegE]
    [If RegE RegE RegE])
   (: list->bit-list : (Listof Any) -> Bit-List)
   (define (list->bit-list lst)
     (cond
      [(null? lst) null]
      [(eq? (first lst) 1) (cons 1 (list->bit-list (rest lst)))]
      [else (cons 0 (list->bit-list (rest lst)))]))



   (: parse-sexpr : Sexpr -> RegE)
   (define (parse-sexpr sexpr)
     (match
      sexpr
      [(list 'reg-len '= (number: n) body)
       (if (<= n 0)
         (error 'parse-sexpr "Register length must be at least 1")
         (parse-sexpr-RegL body n))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
   (: parse-sexpr-RegL : Sexpr Number -> RegE)
   (define (parse-sexpr-RegL sexpr reg-len)
     (match
      sexpr
      [(list (and a (or 1 0)) ...)
       (if (eq? (length a) reg-len)
         (Reg (list->bit-list a))
         (error 'parse-sexpr "wrong number of bits in ~s" a))]
      [(list 'and sexpr1 sexpr2)
       (And
        (parse-sexpr-RegL sexpr1 reg-len)
        (parse-sexpr-RegL sexpr2 reg-len))]
      [(list 'or sexpr1 sexpr2)
       (Or (parse-sexpr-RegL sexpr1 reg-len) (parse-sexpr-RegL sexpr2 reg-len))]
      [(list 'shl sexpr1) (Shl (parse-sexpr-RegL sexpr1 reg-len))]
      ['false (Bool #f)]
      ['true (Bool #t)]
      [(symbol: name) (Id name)]
      [(list 'with (list (symbol: name) named) body)
       (With
        name
        (parse-sexpr-RegL named reg-len)
        (parse-sexpr-RegL body reg-len))]
      [(list 'geq? sexpr1 sexpr2)
       (Geq
        (parse-sexpr-RegL sexpr1 reg-len)
        (parse-sexpr-RegL sexpr2 reg-len))]
      [(list 'maj? sexpr1) (Maj (parse-sexpr-RegL sexpr1 reg-len))]
      [(list 'if sexpr1 sexpr2 sexpr3)
       (If
        (parse-sexpr-RegL sexpr1 reg-len)
        (parse-sexpr-RegL sexpr2 reg-len)
        (parse-sexpr-RegL sexpr3 reg-len))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


   (: parse : String -> RegE)
   (define (parse str) (parse-sexpr (string->sexpr str)))



   (test (parse-sexpr-RegL (string->sexpr "{1 1 1 1}") 4) => (Reg '(1 1 1 1)))
   (test (parse-sexpr-RegL (string->sexpr "{0 0 0}") 3) => (Reg '(0 0 0)))
   (test (parse "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0)))
  
   (define-type RES
     [boolean-res Boolean]
     [RegV Bit-List])


   (: subst : RegE Symbol RegE -> RegE)
   (define (subst expr-reg from to)
     (cases
      expr-reg
      [(Reg lst) expr-reg]
      [(And l r) (And (subst l from to) (subst r from to))]
      [(Or l r) (Or (subst l from to) (subst r from to))]
      [(Shl reg) (Shl (subst reg from to))]
      [(Id name) (if (eq? name from) to expr-reg)]
      [(With bound-id named-expr bound-body)
       (With
        bound-id
        (subst named-expr from to)
        (if (eq? bound-id from) bound-body (subst bound-body from to)))]
      [(If l m r) (If (subst l from to) (subst m from to) (subst r from to))]
      [(Maj l) (Maj (subst l from to))]
      [(Geq l r) (Geq (subst l from to) (subst r from to))]
      [(Bool b) expr-reg]))






   (: eval : RegE -> RES)
   (define (eval expr)
     (cases
      expr
      [(Reg reg) (RegV reg)]
      [(And l r) (reg-arith-op bit-and (eval l) (eval r))]
      [(Or l r) (reg-arith-op bit-or (eval l) (eval r))]
      [(Shl l) (RegV (shift-left (RegV->bit-list (eval l))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(With bound-id named-expr bound-body)
       (eval
        (subst bound-body bound-id
         (cases
          (eval named-expr)
          [(boolean-res b) (Bool b)]
          [(RegV regv) (Reg regv)])))]
      [(Bool b) (boolean-res b)]
      [(Geq l r)
       (boolean-res (geq-bitlists? (RegV->bit-list (eval l)) (RegV->bit-list (eval r))))]
      [(Maj l) (boolean-res (majority? (RegV->bit-list (eval l))))]
      [(If b l r)
       (cases
        (eval b)
        [(boolean-res b) (if b (eval l) (eval r))]
        [(RegV list) (if list (eval l) (eval r))])]))
   (: RegV->bit-list : RES -> Bit-List)
   (define (RegV->bit-list reg)
     (cases
      reg
      [(boolean-res rb)
       (error 'RegV->bit-list "have to return a bit-list ~s" reg)]
      [(RegV b) b]))
   (test (RegV->bit-list (RegV '(0 0))) => '(0 0))
   (test (RegV->bit-list (RegV '(0 1))) => '(0 1))
   (test (RegV->bit-list (RegV '(1 0))) => '(1 0))
   (test (RegV->bit-list (RegV '(1 1))) => '(1 1))
   (test
    (RegV->bit-list (boolean-res #t))
    =error>
    "RegV->bit-list: have to return a bit-list (boolean-res #t)")
   (test (RegV->bit-list (RegV '(1 1 1 0))) => '(1 1 1 0))
   (: bit-and : BIT BIT -> BIT)
   (define (bit-and a b) (if (and (= a 1) (= b 1)) 1 0))
   (test (bit-and 0 0) => 0)
   (test (bit-and 0 1) => 0)
   (test (bit-and 1 0) => 0)
   (test (bit-and 1 1) => 1)
   (: bit-or : BIT BIT -> BIT)
   (define (bit-or a b) (if (or (= a 1) (= b 1)) 1 0))
   (test (bit-or 0 0) => 0)
   (test (bit-or 0 1) => 1)
   (test (bit-or 1 0) => 1)
   (test (bit-or 1 1) => 1)
   (: reg-arith-op : (BIT BIT -> BIT) RES RES -> RES)
   (define (reg-arith-op op reg1 reg2)
     (: bit-arith-op : Bit-List Bit-List -> Bit-List)
     (define (bit-arith-op bl1 bl2)
       (cond
        [(and (null? bl1) (null? bl2)) '()]
        [(or (null? bl1) (null? bl2)) (error 'bit-arith-op "different length")]
        [else
         (cons
          (op (first bl1) (first bl2))
          (bit-arith-op (rest bl1) (rest bl2)))]))
     (RegV (bit-arith-op (RegV->bit-list reg1) (RegV->bit-list reg2))))
   (test (reg-arith-op bit-and (RegV '(0 0)) (RegV '(0 0))) => (RegV '(0 0)))
   (test
    (reg-arith-op bit-and (RegV '(0 1 1 1)) (RegV '(1 0 1 1)))
    =>
    (RegV '(0 0 1 1)))
   (test
    (reg-arith-op bit-and (RegV '(1 1 1 1 1)) (RegV '(0 0 0 0 0)))
    =>
    (RegV '(0 0 0 0 0)))
   (test
    (reg-arith-op bit-and (RegV '(1)) (RegV '(0 0)))
    =error>
    "bit-arith-op: different length")
   (test
    (reg-arith-op bit-and (RegV '(1 1)) (RegV '(0 0 0)))
    =error>
    "bit-arith-op: different length")
   (test (reg-arith-op bit-or (RegV '(1)) (RegV '(0))) => (RegV '(1)))
   (test (reg-arith-op bit-or (RegV '(0 0)) (RegV '(0 0))) => (RegV '(0 0)))
   (test
    (reg-arith-op bit-or (RegV '(0 1 1 1)) (RegV '(1 0 1 1)))
    =>
    (RegV '(1 1 1 1)))
   (test
    (reg-arith-op bit-or (RegV '(1)) (RegV '(0 0)))
    =error>
    "bit-arith-op: different length")
   (test
    (reg-arith-op bit-or (RegV '(1 1)) (RegV '(0 0 0)))
    =error>
    "bit-arith-op: different length")
   (: majority? : Bit-List -> Boolean)
   (define (majority? bl)
     (: mag-help : Bit-List Integer Integer -> Boolean)
     (define (mag-help bl n1 n2)
       (cond
        [(null? bl) (if (or (> n1 n2) (= n1 n2)) true false)]
        [else
         (if (eq? (first bl) 1)
           (mag-help (rest bl) (+ n1 1) n2)
           (mag-help (rest bl) n1 (+ n2 1)))]))
     (mag-help bl 0 0))
   (test (majority? '(1)) => true)
   (test (majority? '(1 1)) => true)
   (test (majority? '(0)) => false)
   (test (majority? '(1 1)) => true)
   (test (majority? '(1 1 0 0 0 0 0 1 1 1 1 1 0 0 0)) => false)
   (test (majority? '(0 1 0 1)) => true)
   (test (majority? '(0 1 1 1 1 1 1)) => true)
   (test (majority? '()) => true)
   (test (majority? '(1 1 1 0 0 0)) => true)
   (: geq-bitlists? : Bit-List Bit-List -> Boolean)
   (define (geq-bitlists? bl1 bl2)
     (cond
      [(and (eq? bl1 null) (eq? bl2 null)) true]
      [(or (eq? bl1 null) (eq? bl2 null))
       (error 'geq-bitlists? "different length")]
      [(> (first bl1) (first bl2)) true]
      [(< (first bl1) (first bl2)) false]
      [else (geq-bitlists? (rest bl1) (rest bl2))]))
   (test (geq-bitlists? '(0 0 0) '(0 0 1)) => false)
   (test (geq-bitlists? '(0) '(0)) => true)
   (test (geq-bitlists? '(1 1 1) '(1 0 1)) => true)
   (test (geq-bitlists? '(1 1 1 0) '(1 1 1 1)) => false)
   (test (geq-bitlists? '(0 0 0 0 0) '(0 0 0 1 0)) => false)
   (test (geq-bitlists? '(1 1 1 1 1) '(1 0 1 1 0)) => true)
   (test (geq-bitlists? '(0 1 1 0 1) '(1 1 0 1 1)) => false)
   (test (geq-bitlists? '(1) '(1)) => true)
   (test (geq-bitlists? '() '(1)) =error> "geq-bitlists?: different length")
   (test (geq-bitlists? '(1 1 1 0 1) '(1 1 0 1 1)) => true)
   (test
    (geq-bitlists? '(0 0) '(0 0 1))
    =error>
    "geq-bitlists?: different length")
   (test (geq-bitlists? '() '()) => true)
   (test
    (geq-bitlists? '(0 1 0 0 0) '(0 1 0 0 0 1))
    =error>
    "geq-bitlists?: different length")
   (test (geq-bitlists? '(1 1 1 1) '(1 1 1 0)) => true)
   (: shift-left : Bit-List -> Bit-List)
   (define (shift-left bl) (append (rest bl) (list (first bl))))
   (test (shift-left '(0 0)) => '(0 0))
   (test (shift-left '(0 1 1 1 1)) => '(1 1 1 1 0))
   (test (shift-left '(1 1 1)) => '(1 1 1))
   (test (shift-left '(1 0 0)) => '(0 0 1))
   (test (shift-left '(0 1)) => '(1 0))
   (test (shift-left '(0 1 0 1 0 1)) => '(1 0 1 0 1 0))
   (test (shift-left '(0 1 0)) => '(1 0 0))
   (test (shift-left '(0 0 0 0 0 0 0 1)) => '(0 0 0 0 0 0 1 0))
   (test (shift-left '(0 1 0 0 1 0 0 1)) => '(1 0 0 1 0 0 1 0))
   (test (shift-left '(1 1 1 0)) => '(1 1 0 1))
   (test (shift-left '(0 1 1 0)) => '(1 1 0 0))
   (test (shift-left '(0 0 0 0)) => '(0 0 0 0))
   (: run : String -> Bit-List)
   (define (run string) (RegV->bit-list (eval (parse string))))
   (test (run "{ reg-len =  4  {1 0 0 0}}") => '(1 0 0 0))
   (test
    (run "{ reg-len =  3  {1 0 0 0}}")
    =error>
    "wrong number of bits in (1 0 0 0)")
   (test (run "{ reg-len =  5  {1 0 0 0 0}}") => '(1 0 0 0 0))
   (test (run "{ reg-len = 4  {shl {1 0 0 0}}}") => '(0 0 0 1))
   (test (run "{ reg-len = 4  {shl {0 1 0 0}}}") => '(1 0 0 0))
   (test (run "{ reg-len = 4  {shl {1 1 0 0}}}") => '(1 0 0 1)) 
  