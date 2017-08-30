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

(declare aexp)
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
    #(second %)))

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
