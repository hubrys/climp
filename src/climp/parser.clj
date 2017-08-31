(ns climp.parser
  (:require [climp.combinators :refer :all]))

(def tokentypes [[#"^[ \n\t]+" nil]
                 [#"^#[^\n]*" nil]

                 [#"^\:=" :reserved]
                 [#"^\(" :reserved]
                 [#"^\)" :reserved]
                 [#"^;" :reserved]
                 [#"^\+" :reserved]
                 [#"^-" :reserved]
                 [#"^\*" :reserved]
                 [#"^/" :reserved]
                 [#"^<=" :reserved]
                 [#"^<" :reserved]
                 [#"^>=" :reserved]
                 [#"^>" :reserved]
                 [#"^=" :reserved]
                 [#"^!=" :reserved]
                 [#"^and" :reserved]
                 [#"^or" :reserved]
                 [#"^not" :reserved]
                 [#"^if" :reserved]
                 [#"^then" :reserved]
                 [#"^else" :reserved]
                 [#"^while" :reserved]
                 [#"^do" :reserved]
                 [#"^end" :reserved]

                 [#"^[0-9]+" :int]
                 [#"^[A-Za-z][A-Za-z-]*" :id]])

(declare aexp bexp bexp-term stmt)
(defn kword [kw]
  (reserved kw :reserved))

(def id (tag :id))
(def number (process (tag :int) #(Integer/parseInt %)))
(defn aexp-value []
  (one-of (process number #(list :int-aexp %))
          (process id #(list :var-aexp %))))

(defn aexp-group []
  (process
    (concatenate (kword "(") (lazy aexp) (kword ")"))
    second))

(defn aexp-term []
  (one-of (aexp-value) (aexp-group)))

(defn any-op [ops]
  (apply one-of (map #(kword %) ops)))

(def aexp-precedence-levels [["*" "/"]
                             ["+" "-"]])

(defn precedence [value-parser levels reducer]
  (loop [parser value-parser
         remaining-levels levels]
    (if (empty? remaining-levels)
      parser
      (recur (expression parser (any-op (first remaining-levels)) reducer)
             (rest remaining-levels)))))

(defn binop-reducer [sep l r]
  (list :binop-aexp sep l r))

(defn aexp []
  (precedence (aexp-term) aexp-precedence-levels binop-reducer))


;;; Binary Operation Parsers
(defn bexp-relop []
  (process
    (concatenate (aexp)
                 (any-op ["<" "<=" ">" ">=" "=" "!="])
                 (aexp))
    #(list :relop-bexp (second %) (first %) (nth % 2))))

(defn bexp-not []
  (process
    (concatenate (kword "not") (lazy bexp-term))
    #(list :not-bexp (second %))))

(defn bexp-group []
  (process
    (concatenate (kword "(") (lazy bexp) (kword ")"))
    second))

(defn bexp-term []
  (one-of (bexp-not) (bexp-relop) (bexp-group)))

(def bexp-precedence-levels [["and"] ["or"]])

(defn sep->boolop [op]
  (case op
    "and" :and-bexp
    "or" :or-bexp))

(defn binary-reducer [sep l r]
  #(list (sep->boolop sep) l r))

(defn bexp []
  (precedence (bexp-term) bexp-precedence-levels binary-reducer))


;;; Statement Parsers

(defn stmt-assign []
  (process (concatenate id (kword ":=") (aexp))
           #(list :assign-stmt (first %) (nth % 2))))

(defn stmt-list []
  (expression (stmt) (kword ";") #(list :stmt-compound %2 %3)))

(defn stmt-if []
  (process (concatenate (kword "if") (bexp)
                        (kword "then") (lazy stmt-list)
                        (optional (concatenate (kword "else") (lazy stmt-list)))
                        (kword "end"))
           (fn [[_ condition _ true-body [_ false-body] -]]
             (list :stmt-if condition true-body false-body))))

(defn stmt-while []
  (process (concatenate (kword "while") (bexp)
                        (kword "do") (lazy stmt-list)
                        (kword "end"))
           (fn [[_ condition _ body _]]
             (list :stmt-while condition body))))

(defn stmt []
  (one-of (stmt-assign)
          (stmt-if)
          (stmt-while)))

(defn parser []
  (phrase (stmt-list)))

(defn parse [tokens]
  (value ((parser) tokens)))


