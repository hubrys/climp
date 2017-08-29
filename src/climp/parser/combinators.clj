(ns climp.parser.combinators)

;;; A combinator is a basic block used for parsing
;;; A combinator is a function that returns a parsing function
;;; A parsing function will either return a result, or
;;; it will return falsey, indicating that it could not parse the input

(defn reserved
  [value tag]
  (fn [[token & rest]]
    (if (and token
             (= (first token) value)
             (= (second token) tag))
      [(first token) rest]
      nil)))

(defn tag
  [tag]
  (fn [[token]]
    (if (and (not (nil? token))
             (= (second token) tag))
      (first token)
      nil)))


;(defn concat
;  [left right]
;  (fn [tokens]
;    (let [lvalue (left tokens)]))
