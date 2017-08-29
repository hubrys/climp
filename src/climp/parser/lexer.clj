(ns climp.parser.lexer)

(defn- next-token
  [input token-types]
  (some #(if (first %) % nil)
        (map
          #(vector (re-find (first %) input) (second %))
          token-types)))

(defn tokenize
  "Generates token list for given input and token list"
  ([input token-types] (tokenize input token-types []))
  ([input token-types results]
   (if (empty? input)
     results
     (let [token (next-token input token-types)]
       (if (nil? token)
         (throw (Exception. "Could not tokenize"))
         (recur (subs input (count (first token))) token-types
                (if (nil? (second token))
                  results
                  (conj results token))))))))
