;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular)

(derive! #'h ::range ::not-empty)

(defn cinc [c]
  (char (inc (int c))))

(defn cdec [c]
  (char (dec (int c))))

(defmacro cle [& forms]
  `(<= ~@(for [f forms] `(int ~f))))

(defmacro cge [& forms]
  `(>= ~@(for [f forms] `(int ~f))))

(defmacro cgt [& forms]
  `(> ~@(for [f forms] `(int ~f))))

(defmacro clt [& forms]
  `(< ~@(for [f forms] `(int ~f))))

(defn ->range
  "A contiguous range of values.

  All characters must be in the UTF-8 range."
  [lower upper]
  {:pre [(char? lower)
         (char? upper)
         (cle lower upper)]}
  {:tag        ::range
   :multi-byte (or (multibyte? upper)
                   (multibyte? lower))
   :lower      lower
   :upper      upper})

(defmethod multibyte? ::range [{:keys [upper lower multi-byte]}]
  (or (multibyte? upper)
      (multibyte? lower)))

(defmethod score ::range [_]
  2)

(defmethod union* [::range ::range] [l r]
  {:pre [;; Check tag and ordering invariants
         ;; for L
         (= (:tag l) ::range)
         (cge (:upper r) (:lower r))
         ;; for R
         (= (:tag r) ::range)
         (cge (:upper l) (:lower l))]}
  (let [;; Provide the invariant that l is the "greater" of the two by upper bound.
        [l r] (cond
                ;; L strictly greater than R
                (cgt (:upper l) (:upper r))
                [l r]

                ;; L and R "equal", sort by lower bound.
                (= (:upper l) (:upper r))
                (if (cge (:lower l) (:lower r))
                  [l r] [r l])

                ;; R strictly greater
                :else
                [r l])
        {r-upper :upper r-lower :lower :as r} r
        {l-upper :upper l-lower :lower :as l} l]
    (cond (= l r)
          ;; Trivial case of equal / totally overlapping regions
          l

          (cle l-lower (cinc r-upper))
          ;; Ranges are continuous, possibly overlapping. Build a bigger range.
          (->range r-lower l-upper)

          (cgt l-lower r-upper)
          ;; L and R are disjoint, non-continuous. Build a set.
          (->union l r)

          ;; There is no other possible case because for l-upper to be LESS than r-lower, either r
          ;; is invalid or r is strictly greater than l in which case our sorting invariant is
          ;; violated.
          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

(defmethod-commutative union* [::character ::range] [a b]
  (union* (->range a a) b))

(defmethod subtraction* [::range ::range] [l r]
  {:pre [;; Check tag and ordering invariants
         ;; for L
         (= (:tag l) ::range)
         (cge (:upper r) (:lower r))
         ;; for R
         (= (:tag r) ::range)
         (cge (:upper l) (:lower l))]}
  (let [{r-upper :upper r-lower :lower :as r} r
        {l-upper :upper l-lower :lower :as l} l]
    (cond (or (cgt l-lower r-upper)
              (cgt r-lower l-upper))
          ;; 1) Trivial case of non-overlapping regions. L looses nothing.
          l

          (and (cge r-upper l-upper)
               (cle r-lower l-lower))
          ;; 2) R fully masks L.
          (->empty)

          (and (cge r-upper l-upper r-lower)
               (cge (cdec r-lower) l-lower))
          ;; 3) Some sub-range of L exists below R's lower bound
          (->range (cdec r-lower) l-lower)

          (and (cge l-upper r-upper l-lower)
               (cge l-lower r-lower))
          ;; 4) Some sub-range of L exists above R's upper bound
          (->range (cinc r-upper) l-upper)

          (cge l-upper (cinc r-upper) (cdec r-lower) l-lower)
          ;; R is completely within L
          (->union
           (->range (cinc r-upper) l-upper )
           (->range l-lower (cdec r-lower)))

          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

(defmethod subtraction* [::range ::character] [a b]
  (subtraction* a (->range b b)))

(defmethod subtraction* [::character ::range] [a b]
  (subtraction* (->range a a) b))

(defmethod intersection* [::range ::range] [l r]
  (let [;; Provide the invariant that l is the "greater" of the two by upper bound.
        [l r] (cond
                ;; L strictly greater than R
                (cgt (:upper l) (:upper r))
                [l r]

                ;; L and R "equal", sort by lower bound.
                (= (:upper l) (:upper r))
                (if (cge (:lower l) (:lower r))
                  [l r] [r l])

                ;; R strictly greater
                :else
                [r l])
        ;; Destructuring for convenience
        {lupper :upper llower :lower} l
        {rupper :upper rlower :lower} r]
    (cond (cgt llower rupper)
          ;; There is a strict ordering on the ranges, one entirely above, one entirely below.
          (->empty)

          (cge lupper rupper llower rlower)
          ;; L is above R but there is overlap
          (->range llower rupper)

          (cge lupper rupper rlower llower)
          r

          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

(defmethod-commutative intersection* [::range ::character] [a b]
  (let [{:keys [upper lower]} a]
    (if (cle lower b upper) b
        (->empty))))

;; Default intersection? is fine
