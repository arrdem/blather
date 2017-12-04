(ns irregular-test
  (:require [clojure.test :as t]
            [irregular.core :refer :all]))

(t/deftest test-subtraction-contains
  (t/are [example result] (t/is (= example result))
    (subtraction (->range \a \z) \z) (->range \a \y)
    (subtraction (->range \a \z) \b) (union \a (->range \c \z))))
