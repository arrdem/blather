;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)

(derive! #'h ::character ::not-empty)

(defmethod multibyte? ::character [i]
  (> (int i) 127))

(defmethod score ::character [_]
  1)

(defmethod union* [::character ::character] [a b]
  (cond (= a b)                   a
        ;; Collapse consecutive characters into ranges
        (= (int a) (inc (int b))) (->range b a)
        (= (int a) (dec (int b))) (->range a b)
        :else                     (->union a b)))

(defmethod-commutative union* [::character ::union] [a b]
  (update b :terms conj a))

(defmethod intersection* [::character ::character] [a b]
  (if (= a b) a
      (->union)))

(defmethod intersects? [::character ::character] [a b]
  (= a b))

;; Default subtract* via intersects? being sane
