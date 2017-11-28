(ns lark-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing]]
            [blather.lark
             :refer [-parser]
             :rename {-parser parser}]
            [instaparse.core :refer [parse]]))

(deftest test-calc-parses
  (testing "Parse the calc grammar."
    (parse parser (slurp "etc/calc.lark")))

  (testing "Parse the tree grammar."
    (parse parser (slurp "etc/tree.lark")))

  (testing "Parse the python3 grammar."
    (parse parser (slurp "etc/python3.lark"))))
