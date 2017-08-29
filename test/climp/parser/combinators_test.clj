(ns climp.parser.combinators-test
  (:use midje.sweet)
  (:require [climp.parser.combinators :refer :all]))

(facts "about `reserved`"
       ((reserved "+" :reserved) [["+" :reserved]]) => ["+" nil]
       ((reserved "+" :reserved) [["+" :reserved] ["123" :int]]) => ["+" [["123" :int]]]
       ((reserved "+" :reserved) [["-" :reserved]]) => falsey
       ((reserved "+" :reserved) [["+" :int]]) => falsey)


(facts "about `tag`"
       ((tag :int) [["123" :int]]) => "123"
       ((tag :int) [["asdf" :string]]) => nil
       ((tag :string) [["asdf" :string]]) => "asdf")


