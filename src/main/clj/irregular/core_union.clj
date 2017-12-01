;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)
(require 'clojure.set)

(derive! #'h ::union ::not-empty)

(defn ->union
  "Raw union structure constructor. `#'union` should probably be preferred."
  ([]
   (->empty))
  ([t & terms]
   {:tag ::union :terms (conj (set terms) t)}))

(defn union?
  "Predicate. True iff the given structure is a union of sets."
  [{:keys [tag]}]
  (= tag ::union))

;; A union is multi-byte if any of its terms are.
(defmethod multibyte? ::union [a]
  (some multibyte? (:terms a)))

(defmethod score ::union [{:keys [terms]}]
  (apply + 1 (map score terms)))

;; In the general case, we just stick both in a union and sort it out later if ever.
(defmethod union* [::not-empty ::not-empty] [a b]
  (if (= a b) a
      (->union a b)))

;; In the case of taking a union with a union, try to simplify.
(defmethod union* [::union ::union] [a b]
  (apply union (concat (:terms a) (:terms b))))

(defmethod-commutative union* [::not-empty ::union] [a b]
  (let [{:keys [terms]} b]
    (loop [a              a
           [term & terms] (seq terms)
           discards       []]
      (if-not term
        ;; Default behavior
        (->union union a (:terms b))
        (let [candidate (union a term)]
          (if-not (union? candidate)
            ;; We were able to find a simplification somehow!
            ;; Take it and give it a chance to reduce against everything else.
            (recur candidate (concat discards terms) [])
            ;; give the others a chance
            (recur a terms (conj discards term))))))))

;; Two unions intersect if their sets intersect.
(defmethod intersection* [::union ::union] [a b]
  (apply union (clojure.set/intersection (:terms a) (:terms b))))

(defmethod intersection* [::union ::not-empty] [a b]
  (->> (map (partial intersection* b) (:terms a))
       (apply union)))

;; A union will intersect with anything if the other thing intersects with any of its components
(defmethod intersects? [::union ::union] [a b]
  (boolean (some (:terms a) (:terms b))))

(defmethod intersects? [::union ::not-empty] [a b]
  (boolean (some (partial intersects? b) (:terms a))))

;; Subtraction is a bit tricky
(defmethod subtraction* [::union ::union] [a b]
  (apply union (remove (:terms b) (:terms a))))

(defmethod subtraction* [::union ::not-empty] [a b]
  (apply union (disj (:terms a) b)))

(defmethod subtraction* [::not-empty ::union] [a b]
  (apply subtraction a (:terms b)))
