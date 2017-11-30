;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular)

(derive! #'h ::intersection ::not-empty)

(defn ->intersection
  "Raw intersection structure constructor. `#'intersection` should probably be preferred."
  [& terms]
  {:tag   ::intersection
   :terms (set terms)})

(defn intersection?
  "Predicate. True iff the given structure is an intersection of sets."
  [{:keys [tag]}]
  (= tag ::intersection))

;; An intersection COULD BE multibyte if any if its terms are. We may not be able to figure that out
;; statically because character classes and fuckery.
(defmethod multibyte? ::intersection [a]
  (boolean (some multibyte? (:terms a))))

(defmethod score ::intersection [{:keys [terms]}]
  (apply + 2 (map score terms)))

(defmethod intersection* [::any ::any] [a b]
  (if (= a b) a
      (->intersection a b)))

(defmethod intersection* [::intersection ::intersection] [a b]
  (apply intersection (concat (:terms a) (:terms b))))

(defmethod-commutative intersection* [::intersection ::any] [a b]
  (apply intersection b (:terms a)))

;; By default only equal structures intersect
(defmethod intersects? [::not-empty ::not-empty] [a b]
  (not= (->empty) (intersection a b)))

;; Intersection structures intersect if any of their terms intersect
(defmethod-commutative intersects? [::intersection ::not-empty] [a b]
  (every? (partial intersects? b) (:terms a)))
