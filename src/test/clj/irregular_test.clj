(ns irregular-test
  (:require [clojure.test :as t]
            [irregular.core :as i]))

(def examples
  [(i/->empty)
   (i/->range \a \z)
   (i/->equiv \f)
   (i/->collation \a)
   (i/->subtraction (i/->equiv \f) \f)
   (i/->union (i/->equiv \f) \1)
   \a])

(t/deftest test-binary-op-api-completeness
  (let [methods [#'i/union #'i/intersection #'i/intersects? #'i/subtraction]]
    (doseq [m methods]
      (t/testing (format "Testing method %s" m)
        (doseq [a examples
                b examples]
          (t/testing (format "Testing example (%s %s)" a b)
            (t/is (do (m a b) true))))))))

(t/deftest test-unary-op-api-completeness
  (let [methods [#'i/score #'i/multibyte?]]
    (doseq [m methods]
      (t/testing (format "Testing method %s" m)
        (doseq [a examples]
          (t/testing (format "Testing example %s" a)
            (t/is (do (m a) true))))))))
