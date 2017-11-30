(ns rfc5234-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing]]
            [languages.rfc5234 :refer [parse]]))

(deftest test-self-parses
  (testing "Tests that the RFC5234 parser can parse its own RFC5234 structured specification."
    ;; FIXME: $CWD dependent tests aren't great, but the RFC contents sorta belong in /etc
    (parse (slurp (io/file "etc/rfc5234.txt")))))
