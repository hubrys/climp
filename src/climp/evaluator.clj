(ns climp.evaluator
  [:require [climp.lexer :as lexer]
            [climp.parser :refer [parse]]])

(defn evaluate-int [ast env]
  (second ast))

(defn evaluate-var [ast env]
  (if-let [value (env (second ast))]
    value
    (throw (Exception. (str "no value bound for: " (second ast))))))

(def binop-funcs {"+" +
                  "-" -
                  "*" *
                  "/" /})
(defn evaluate-binop [[_ op l r] env]
  (if-let [binop-func (binop-funcs op)]
    (binop-func (evaluate l env) (evaluate r env))))


(defn evaluate-assign [[_ id expr] env]
  (assoc env id (evaluate expr env)))

(def eval-functions {:int-aexp    evaluate-int
                     :var-aexp    evaluate-var
                     :stmt-assign evaluate-assign
                     :binop-aexp  evaluate-binop})
(defn evaluate [ast env]
  (if-let [eval-func (eval-functions (first ast))]
    (eval-func ast env)
    (throw (Exception. (str "No handler for: " (name (first ast)))))))

