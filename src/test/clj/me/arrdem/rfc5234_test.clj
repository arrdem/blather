(ns me.arrdem.rfc5234-test
  (:require [me.arrdem.blather.rfc5234 :refer [parse]]
            [clojure.test :refer [deftest testing]]
            [clojure.java.io :as io]))

(deftest test-self-parses
  (testing "Tests that the RFC5234 parser can parse its own RFC5234 structured specification."
    (parse (slurp (io/file "etc/rfc5234.txt")))))
