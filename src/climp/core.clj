(ns climp.core
  (:require [climp.lexer :refer [tokenize]]
            [climp.parser :refer [parse tokentypes]]
            [climp.evaluator :refer [evaluate]])
  (:gen-class))

(defn evaluate-text [text]
  (evaluate (parse (tokenize text tokentypes)) {}))

(defn -main
  [filepath]
  (let [text (slurp filepath)
        result (evaluate-text text)]
    (println result)
    result))
