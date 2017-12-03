(ns posix-bre-test
  (:require [clojure.test :as t]
            [languages.posix-bre :refer :all]))

(t/deftest fixme
  (t/are [example] (t/is (parse example))
    "foo"
    "foo|bar"
    "[baz]"
    "[^qux]"
    "[[.z.]]"
    "("
    "\\(()\\)"
    "\\."
    "."))
