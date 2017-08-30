(ns climp.combinators)

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

(defn concatenate
  [& parsers]
  (fn [tokens]
    (loop [rem-parsers parsers
           rem-tokens tokens
           results []]
      (if (empty? rem-parsers)
        [results, rem-tokens]
        (let [parser (first rem-parsers)
              result (parser rem-tokens)]
          (if result
            (recur (rest rem-parsers)
                   (subtokens result)
                   (conj results (value result)))
            nil))))))

(defn one-of
  [& parsers]
  (fn [tokens]
    (loop [remaining-parsers parsers]
      (if (empty? remaining-parsers)
        nil
        (let [parser (first remaining-parsers)
              result (parser tokens)]
          (if result
            result
            (recur (rest remaining-parsers))))))))

(defn optional
  [parser]
  (fn [tokens]
    (let [result (parser tokens)]
      (if result result [nil tokens]))))

(defn repetition
  [parser]
  (fn [tokens]
    (loop [remaining-tokens tokens
           results []]
      (let [result (parser remaining-tokens)]
        (if result
          (recur (subtokens result) (conj results (value result)))
          [results remaining-tokens])))))

(defn process
  [parser processor]
  (fn [tokens]
    (let [result (parser tokens)]
      (if result
        [(processor (value result)) (subtokens result)]
        nil))))

(defn lazy
  [parser-func]
  (fn [tokens]
    ((parser-func) tokens)))

(defn phrase
  [parser]
  (fn [tokens]
    (let [result (parser tokens)]
      (if (empty? (subtokens result))
        result
        nil))))

(defn expression
  [parser separator reducer]
  (fn [tokens]
    (let [result (parser tokens)]
      (loop [current (value result)
             remaining-tokens (subtokens result)]
        (let [sep-result ((concatenate separator parser) remaining-tokens)]
          (if sep-result
            (recur (reducer (first (value sep-result)) current (second (value sep-result)))
                   (subtokens sep-result))
            [current, remaining-tokens]))))))
