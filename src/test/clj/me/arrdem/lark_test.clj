(ns me.arrdem.lark-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing]]
            [me.arrdem.blather.lark :refer [parse]]))

(deftest test-calc-parses
  (testing "Parse the python3 grammar."
    (parse (slurp "https://raw.githubusercontent.com/erezsh/lark/7373993/examples/python3.g"))))
