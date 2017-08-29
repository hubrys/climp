(ns climp.lexer-test
  (:require [clojure.test :refer :all]
            [climp.lexer :refer :all]))

(def token-types [[#"^[ \n\t]+" nil]
                  [#"^[0-9]+"   :int]
                  [#"^\+"       :reserved]])

(deftest tokenize-test
  (testing "tokenize"
    (testing "with simple values"
      (is (= '(("1" :int))
           (tokenize "1" token-types))))
    (testing "with arithmetic expressions"
      (is (= '(("1" :int) ("+" :reserved) ("2" :int))
             (tokenize "1 + 2" token-types))))))
