(ns jdk-re-test
  (:require [clojure.test :as t]
            [languages.jdk-re
             :refer [-parser]
             :rename {-parser parser}]
            [instaparse.core :refer [parse]]))

(defn do-parse [text]
  (parse parser text))

(t/deftest test-char-parses
  (t/is (do-parse "a"))
  (t/is (do-parse "\\{"))
  (t/is (do-parse "\\."))
  (t/is (do-parse "\\n"))
  (t/is (do-parse "\\x5555"))
  (t/is (do-parse "\\0333")))

(t/deftest test-cat-parses
  (t/is (do-parse "ab"))
  (t/is (do-parse "abc")))

(t/deftest test-alt-parses
  (t/is (do-parse "a|b"))
  (t/is (do-parse "a|b|c|d")))

(t/deftest test-group-parses
  (t/is (do-parse "a(b|c)")))

(t/deftest test-charset-parses
  (t/is (do-parse "[a]"))
  (t/is (do-parse "[a-z]"))
  (t/is (do-parse "[A-Za-z]"))
  (t/is (do-parse "[a&&[b]&&[^c]]")))
