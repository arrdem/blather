(ns me.arrdem.jdk-re-test
  (:require [clojure.test :as t]
            [me.arrdem.irregular.jdk-re :refer [parse]]))

(t/deftest test-char-parses
  (t/is (= [:pattern
            [:simple-character "a"]]
           (parse "a")))

  (t/is (= [:pattern
            [:escaped-character
             [:special-character "{"]]]
           (parse "\\{")))

  (t/is (= [:pattern
            [:escaped-character
             [:special-character "."]]]
           (parse "\\.")))

  (t/is (= [:pattern
            [:escaped-character
             [:named-character "n"]]]
           (parse "\\n")))

  (t/is (= [:pattern
            [:escaped-character
             [:codepoint
              [:hex-codepoint "5555"]]]]
           (parse "\\x5555")))

  (t/is (= [:pattern
            [:escaped-character
             [:codepoint
              [:octal-codepoint "333"]]]]
           (parse "\\0333"))))

(t/deftest test-cat-parses
  (t/is (= [:pattern
            [:concatenation
             [:simple-character "a"]
             [:simple-character "b"]]]
           (parse "ab")))

  (t/is (= [:pattern
            [:concatenation
             [:simple-character "a"]
             [:concatenation
              [:simple-character "b"]
              [:simple-character "c"]]]]
           (parse "abc"))))

(t/deftest test-charset-parses
  (t/is (parse "[a]"))
  (t/is (parse "[a-z]"))
  (t/is (parse "[A-Za-z]"))
  (t/is (parse "[a&&[b]&&[^c]]")))


