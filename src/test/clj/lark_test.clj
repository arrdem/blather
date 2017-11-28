(ns lark-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing]]
            [blather.lark :refer [parse]]))

(deftest test-calc-parses
  (testing "Parse the calc grammar."
    (parse (slurp "etc/calc.lark")))

  (testing "Parse the tree grammar."
    (parse (slurp "etc/tree.lark")))

  (testing "Parse the python3 grammar."
    (parse (slurp "etc/python3.lark"))))
