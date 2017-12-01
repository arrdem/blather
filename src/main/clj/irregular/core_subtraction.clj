;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)

(derive! #'h ::subtraction ::not-empty)

(defn ->subtraction
  "Raw subtraction structure constructor. `#'subtraction` should probably be preferred."
  ([minuend]
   minuend)
  ([minuend s & subtrahends]
   {:tag ::subtraction

    ;; God this terminology is awful
    :minuend     minuend
    :subtrahends (conj (set subtrahends) s)}))

(defn subtraction?
  "Predicate. True iff the given structure is a subtraction of sets."
  [{:keys [tag]}]
  (= tag ::subtraction))

(defmethod score ::subtraction [{:keys [minuend subtrahends]}]
  (apply + 2 (score minuend) (map score subtrahends)))

(defmethod multibyte? ::subtraction [{:keys [minuend subtrahends]}]
  (or (multibyte? minuend)
      (some multibyte? subtrahends)))

;; FIXME: subtraction uses the DEFAULT implementation of union.

(defmethod subtraction* [::any ::any] [a b]
  (if (intersects? a b)
    (->subtraction a b)
    a))

(defmethod intersects? [::subtraction ::not-empty] [{:keys [minuend subtrahends]} b]
  (and (intersects? minuend b)
       (not (some (partial intersects? b) subtrahends))))

(defmethod intersection* [::subtraction ::subtraction] [a b]
  (let [{a* :minuend b* :subtrahends} a
        {c* :minuend d* :subtrahends} b]
    (->subtraction (intersection a* c*)
                   (union {:tag ::union :terms b*} {:tag ::union :terms d*}))))

(defmethod intersection* [::subtraction ::not-empty] [a b]
  (let [{:keys [minuend subtrahends]} a]
    (apply subtraction (intersection minuend b) subtrahends)))
