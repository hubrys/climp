(ns climp.parser.combinators-test
  (:use midje.sweet)
  (:require [climp.parser.combinators :refer :all]
            [climp.parser.lexer :refer [tokenize]]))

(def token-types [[#"^[ \n\t]+" nil]
                  [#"^[0-9]+" :int]
                  [#"^\+" :reserved]
                  [#"^\-" :reserved]
                  ])

(def parse #(tokenize % token-types))

(facts "about `reserved`"
       ((reserved "+" :reserved) (parse "+")) => ["+" nil]
       ((reserved "+" :reserved) (parse "+ 123")) => ["+" (parse "123")]
       ((reserved "+" :reserved) (parse "-")) => falsey
       ((reserved "+" :reserved) [["+" :int]]) => falsey)


(facts "about `tag`"
       ((tag :int) [["123" :int]]) => "123"
       ((tag :int) [["asdf" :string]]) => nil
       ((tag :string) [["asdf" :string]]) => "asdf")


