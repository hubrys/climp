(ns climp.evaluator
  [:require [climp.lexer :as lexer]
            [climp.parser :refer [parse]]])

(declare evaluate)
(defn evaluate-int [[_ value] env]
  value)

(defn evaluate-var [[_ id] env]
  (env id 0))

(def binop-funcs {"+" +
                  "-" -
                  "*" *
                  "/" /})
(defn evaluate-binop [[_ op l r] env]
  (if-let [binop-func (binop-funcs op)]
    (binop-func (evaluate l env) (evaluate r env))))

(defn evaluate-not [[_ expr] env]
  (not (evaluate expr env)))

(def relop-funcs {"<"  <
                  "<=" <=
                  ">"  >
                  ">=" >=
                  "="  =
                  "!=" not=})
(defn evaluate-relop [[_ op left right] env]
  (if-let [relop-func (relop-funcs op)]
    (relop-func (evaluate left env) (evaluate right env))))

(defn evaluate-bool [[op left right] env]
  (if (= op :and-bexp)
    (and (evaluate left env) (evaluate right env))
    (or (evaluate left env) (evaluate right env))))

(defn evaluate-assign [[_ id expr] env]
  (assoc env id (evaluate expr env)))

(defn evaluate-compound [[_ left right] env]
  (evaluate right (evaluate left env)))

(defn evaluate-if [[_ condition true-body false-body] env]
  (if (evaluate condition env)
    (evaluate true-body env)
    (evaluate false-body env)))

(defn evaluate-while [[_ condition body] env]
  (loop [env' env]
    (if (evaluate condition env')
      (recur (evaluate body env'))
      env')))

(def eval-functions {:int-aexp      evaluate-int
                     :var-aexp      evaluate-var
                     :stmt-assign   evaluate-assign
                     :binop-aexp    evaluate-binop
                     :relop-bexp    evaluate-relop
                     :not-bexp      evaluate-not
                     :and-bexp      evaluate-bool
                     :or-bexp       evaluate-bool
                     :stmt-if       evaluate-if
                     :stmt-compound evaluate-compound
                     :stmt-while    evaluate-while})
(defn evaluate [ast env]
  (if-let [eval-func (eval-functions (first ast))]
    (eval-func ast env)
    (throw (Exception. (str "No handler for: " (name (first ast)))))))

