(ns climp.parser.combinators-test
  (:use midje.sweet)
  (:require [climp.combinators :refer :all]
            [climp.lexer :refer [tokenize]]))

(def token-types [[#"^[ \n\t]+" nil]
                  [#"^[0-9]+" :int]
                  [#"^[A-Za-z][A-Za-z-]*" :id]
                  [#"^\+" :reserved]
                  [#"^\-" :reserved]])


(def parse #(tokenize % token-types))

(facts "about `reserved`"
       ((reserved "+" :reserved) (parse "+")) => ["+" nil]
       ((reserved "+" :reserved) (parse "+ 123")) => ["+" (parse "123")]
       ((reserved "+" :reserved) (parse "-")) => falsey
       ((reserved "+" :reserved) [["+" :int]]) => falsey)


(facts "about `tag`"
       ((tag :int) (parse "123")) => ["123" nil]
       ((tag :int) [["asdf" :string]]) => nil
       ((tag :id) (parse "asdf")) => ["asdf" nil])
