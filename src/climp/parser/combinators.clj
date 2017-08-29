(ns climp.parser.combinators)

;;; A combinator is a basic block used for parsing
;;; A combinator is a function that returns a parsing function
;;; A parsing function will either return a result, or
;;; it will return falsey, indicating that it could not parse the input

(defn subtokens [parser-result] (second parser-result))
(defn value [parser-result] (first parser-result))

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
  (fn [[token & rest]]
    (if (and (not (nil? token))
             (= (second token) tag))
      [(first token) rest]
      nil)))


(defn- concatenate-helper
  [last-result parser]
  (println last-result)
  (let [result (parser (subtokens last-result))]
    (println "subtokens" result)
    [(conj (value last-result) (value result)) (subtokens result)]))

(defn concatenate
  [& parsers]
  (fn [tokens]
    (let [result (reduce concatenate-helper [[] tokens] parsers)]
      (if (some nil? (value result))
        nil
        result)))
  )

;(defn concatenatee
;  [left right]
;  (fn [tokens]
;    (let [lvalue (left tokens)
;          rvalue (if lvalue (right (second lvalue)) nil)]
;      (if rvalue
;        [[lvalue rvalue] (second rvalue)]
;        nil)))
;  [left & rest])

(defn alternate
  [left right])
