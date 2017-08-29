(ns climp.parser.lexer-test
  (:use midje.sweet)
  (:require [clojure.test :refer :all]
            [climp.parser.lexer :refer [tokenize]]))

(def token-types [[#"^[ \n\t]+" nil]
                  [#"^[0-9]+" :int]
                  [#"^\+" :reserved]])

(facts "about 'tokenize'"
       (tokenize "1" token-types) => `(("1" :int))
       (tokenize "1 + 2" token-types) => '(("1" :int) ("+" :reserved) ("2" :int))
       (tokenize "&" token-types) => (throws Exception))
