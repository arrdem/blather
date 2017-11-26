(ns me.arrdem.irregular.char-sets
  "Possibly unicode character sets as [upper, ... lower] pairs & operators thereon."
  (:require [me.arrdem.irregular :as i]
            [me.arrdem.irregular.char :as char]))

;; We're gonna support all character values, being positive integers between `0` and
;; `Character/MAX_CODE_POINT`. A character set then is a set of characters which we'll allow to
;; match at a given terminal. The traditional encoding is lists of ranges (or singletons), which
;; represent either included or excluded ranges of the codepoint or byte space.
;;
;; What we want to use (or emit) then is the most compact set representation for all characters
;; which will match.
(defn char-range
  "Implementation detail of character sets.
  o
  Defines a character range with an upper and lower bound.

  The bounds may be equal if only one char is matched."
  [lower upper]
  {:pre [(or (integer? lower) (char? lower))
         (or (integer? upper) (char? upper))]}
  (let [upper* (max (int upper) (int lower))
        lower* (min (int upper) (int lower))]
    {:tag        ::char-range
     :multi-byte (or (char/multibyte? upper)
                     (char/multibyte? lower))
     :upper      upper*
     :lower      lower*}))

(defmethod char/multibyte? ::char-range [{:keys [upper lower]}]
  (or (char/multibyte? upper)
      (char/multibyte? lower)))

(defn char
  "Alias for `(char-range c c)`. Defines a character range which only matches one thing."
  [c]
  (char-range c c))

(defn char-range-union
  "Implementation detail.

  Simplifying union of two character ranges.

  DOES NOT SUPPORT CHARACTER SETS. `#'char-set-union` should be universally preferred."
  [l r]
  {:pre [;; Check tag and ordering invariants
         ;; for L
         (= (:tag l) ::char-range)
         (>= (:upper r) (:lower r))
         ;; for R
         (= (:tag r) ::char-range)
         (>= (:upper l) (:lower l))]}
  (let [;; Provide the invariant that l is the "greater" of the two by upper bound.
        [l r] (cond
                ;; L strictly greater than R
                (> (:upper l) (:upper r))
                [l r]

                ;; L and R "equal", sort by lower bound.
                (= (:upper l) (:upper r))
                (if (>= (:lower l) (:lower r))
                  [l r] [r l])

                ;; R strictly greater
                :else
                [r l])
        {r-upper :upper r-lower :lower :as r} r
        {l-upper :upper l-lower :lower :as l} l]
    (cond (= l r)
          ;; Trivial case of equal / totally overlapping regions
          l

          (<= l-lower (inc r-upper))
          ;; Ranges are continuous, possibly overlapping. Build a bigger range.
          {:tag        ::char-range
           :multi-byte (or (char/multibyte? l-upper)
                           (char/multibyte? r-lower))
           :upper      l-upper
           :lower      r-lower}

          (> l-lower r-upper)
          ;; L and R are disjoint, non-continuous. Build a set.
          {:tag        ::char-set
           :multi-byte (or (char/multibyte? l)
                           (char/multibyte? r))
           :ranges     [r l]}

          ;; There is no other possible case because for l-upper to be LESS than r-lower, either r
          ;; is invalid or r is strictly greater than l in which case our sorting invariant is
          ;; violated.
          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

(defmethod char/multibyte? ::char-set [{:keys [ranges]}]
  (some char/multibyte? ranges))

(def EMPTY-CHAR-SET
  {:tag ::char-set :multi-byte false :ranges []})

(defn char-set* [range-or-set]
  (case (:tag range-or-set)
    (::char-range) [range-or-set]
    (::char-set)   (mapcat char-set* (:ranges range-or-set))
    (throw (IllegalStateException. (str range-or-set)))))

(defn char-set-union
  "Recursively converts a list of character ranges and/or sets to a single new character set."
  [& ranges-and-sets]
  {:pre [(every? #(#{::char-range ::char-set} (:tag %)) ranges-and-sets)]}
  (->> (mapcat char-set* ranges-and-sets)
       (sort-by :lower)
       (sort-by :upper)
       (reduce (fn
                 ([] EMPTY-CHAR-SET)
                 ([a] a)
                 ([a b]
                  (if (and (= (:tag a) ::char-set)
                           (not (:ranges a)))
                    ;; The easy case, we got nothing on the left and something on the right
                    b
                    ;; The hard case, we have either a range or a set on the left and a range on the
                    ;; right. So we take the range or the last of the ranges (being the only one
                    ;; that could intersect thanks to the sorting property from above) and intersect
                    ;; those two.
                    ;;
                    ;; If we started with ranges and got a range, we take the resulting range.
                    ;; If we started with a range and we got a set, we take the resulting set.
                    ;; If we started with a set and we got a new set back, we 
                    (let [a*  (case (:tag a)
                                ::char-range a
                                ::char-set   (last (:ranges a)))
                          res (char-range-union a* b)]
                      (case [(:tag a) (:tag res)]
                        ([::char-range ::char-range]
                         [::char-range ::char-set])
                        res

                        ([::char-set ::char-range])
                        ;; The last two ranges condensed.
                        (let [ranges `[~@(butlast (:ranges a)) ~res]]
                          {:tag        ::char-set
                           :multi-byte (some char/multibyte? ranges)
                           :ranges     ranges})

                        ([::char-set ::char-set])
                        ;; The last two ranges were disjoint
                        (let [ranges `[~@(:ranges a) ~b]]
                          {:tag        ::char-set
                           :multi-byte (or (char/multibyte? a)
                                           (char/multibyte? b))
                           :ranges     ranges})))))))))

(defn char-range-difference*
  "Implementaton detail.

  Subtracts the right character range from the left character range.

  Returns either a new character range, or a character set."
  [l r]
  {:pre [;; Check tag and ordering invariants
         ;; for L
         (= (:tag l) ::char-range)
         (>= (:upper r) (:lower r))
         ;; for R
         (= (:tag r) ::char-range)
         (>= (:upper l) (:lower l))]}
  (let [{r-upper :upper r-lower :lower :as r} r
        {l-upper :upper l-lower :lower :as l} l]
    (cond (or (> l-lower r-upper)
              (> r-lower l-upper))
          ;; 1) Trivial case of non-overlapping regions. L looses nothing.
          l

          (and (>= r-upper l-upper)
               (<= r-lower l-lower))
          ;; 2) R fully masks L.
          EMPTY-CHAR-SET

          (and (>= r-upper l-upper r-lower)
               (>= (dec r-lower) l-lower))
          ;; 3) Some sub-range of L exists below R's lower bound
          (char-range (dec r-lower) l-lower)

          (and (>= l-upper r-upper l-lower)
               (>= l-lower r-lower))
          ;; 4) Some sub-range of L exists above R's upper bound
          (char-range l-upper (inc r-upper))

          (>= l-upper (inc r-upper) (dec r-lower) l-lower)
          ;; R is completely within L
          {:tag        ::char-set
           :multi-byte (or (char/multibyte? l)
                           (char/multibyte? r))
           :ranges     [(char-range l-upper (inc r-upper))
                        (char-range (dec r-lower) l-lower)]}

          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

(defn char-set-difference*
  "Implementation detail.

  Returns a new character range or character set encoding the
  subtraction of the right range or set from the left range or set."
  [{:keys [ranges] :as l} r]
  (let [new-ranges (->> ranges
                        (map #(char-range-difference* % r))
                        (mapcat char-set*)
                        (sort-by :lower)
                        (sort-by :upper)
                        vec)]
    {:tag        ::char-set
     :multi-byte (some char/multibyte? new-ranges)
     :ranges     new-ranges}))

(defn char-set-difference
  "Subtracts a number of character ranges & character sets from the left character range or set."
  [l & ranges-and-sets]
  {:pre [(every? #(#{::char-range ::char-set} (:tag %)) ranges-and-sets)]}
  (->> (mapcat char-set* ranges-and-sets)
       (sort-by :lower)
       (sort-by :upper)
       (reduce (fn
                 ([l] l)
                 ([l e]
                  (case (:tag l)
                    (::char-range) (char-range-difference* l e)
                    (::char-set)   (char-set-difference* l e))))
               l)))

(def ANY-CHAR-RANGE
  "Matches any character whatsoever."
  (char-range 0 Character/MAX_CODE_POINT))

(def NOT-NULL-CHAR-RANGE
  "Matches any character BUT the NULL (0) character."
  (char-range 1 Character/MAX_CODE_POINT))

(defn char-set-negate
  "Returns the character set which matches everything the given character set (or range) rejects"
  [range-or-set]
  (apply char-set-difference* ANY-CHAR-RANGE (char-set* range-or-set)))

(defn char-range-intersection
  "Implementation detail.

  Given a pair of character ranges L and R, returns the range which
  they share, or `#'EMPTY-CHAR-SET` if there is no overlap."
  [l r]
  (let [;; Provide the invariant that l is the "greater" of the two by upper bound.
        [l r] (cond
                ;; L strictly greater than R
                (> (:upper l) (:upper r))
                [l r]

                ;; L and R "equal", sort by lower bound.
                (= (:upper l) (:upper r))
                (if (>= (:lower l) (:lower r))
                  [l r] [r l])

                ;; R strictly greater
                :else
                [r l])
        ;; Destructuring for convenience
        {lupper :upper llower :lower} l
        {rupper :upper rlower :lower} r]
    (cond (> llower rupper)
          ;; There is a strict ordering on the ranges, one entirely above, one entirely below.
          EMPTY-CHAR-SET

          (>= lupper rupper llower rlower)
          ;; L is above R but there is overlap
          (char-range rupper llower)

          (>= lupper rupper rlower llower)
          r

          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

(defn char-set-intersection
  "Returns the intersection of the left set/range with the right set/range"
  [l & ranges-and-sets]
  {:pre [(every? #(#{::char-range ::char-set} (:tag %)) ranges-and-sets)]}
  ;; FIXME: Naive but correct implementation. Compute the sequence of ranges constituting the left
  ;; and right character sets. Then compute the CROSS PRODUCT of overlapping ranges, and union the
  ;; resulting overlapping ranges together. Union is a monoid, so we don't have to special case
  ;; repetition or anything else and we're order insensitive. We just do more work than strictly
  ;; required. It's fine, cores are fast.
  (->> (for [l     (char-set* l)
             r     (mapcat char-set* ranges-and-sets) 
             :let  [i (char-range-intersection l r)]
             :when (not= i EMPTY-CHAR-SET)]
         i)
       (apply char-set-union)))

(defn as-character-class
  "Helper for mapping integer and character values to character classes."
  [range-char-or-jdk]
  (cond (instance? Character range-char-or-jdk)
        (char range-char-or-jdk)

        (integer? range-char-or-jdk)
        (char range-char-or-jdk)

        :else range-char-or-jdk))
