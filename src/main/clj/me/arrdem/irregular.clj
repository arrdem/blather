(ns me.arrdem.irregular
  "Regular expressions.

  As data structures.

  It's a rite of passage I guess."
  (:refer-clojure :exclude [cat char])
  (:require [clojure.string :refer [join]]))

(defmacro debug [fmt & args]
  (binding [*out* *err*]
    (apply printf fmt args)))

;; regexp elements:
;;
;; - Concatenation
;; - Alternation (of groups or terms)
;; - Character groups
;;   - Set union
;;   - Set intersection
;;   - Set subtraction
;;   - Set negation (anything but)
;;   - Ranges
;; - Capture groups (named)
;;
;; Some implementations provide named character classes as a convenience. Eg POSIX defines [:foo:]
;; style character classes, GNU family tools provide standardized \w, \s style named groups and some
;; implementations even provide facilities for referring to the Unicode codepages.
;;
;; This is all made harder by the question of what's the basic unit of a regular expression.
;;
;; One reasonable answer is a BYTE, defined to be eight consecutive bits. Usually bytes in the ASCII
;; codepage standard are of interest.
;;
;; Another potentially reasonable answer is a BIT. Some protocols and file layouts are specified in
;; terms of bit formats, although this is less common today.
;;
;; The most relevant answer in modernity perhaps is UTF-8 codepoints. A codepoint is a muti-byte
;; encoding of an integer, naming either a glyph, modifier or element of a glyph.

(defn tag-dx [tagged-or-raw]
  (cond (map? tagged-or-raw)
        (:tag tagged-or-raw ::default)

        (or (char? tagged-or-raw)
            (integer? tagged-or-raw))
        ::integer

        :else
        ::default))

(defmulti multibyte?
  "Determines whether a given value requires multibyte pattern matching."
  #'tag-dx)

(defmethod multibyte? ::default [m]
  false)

(defmethod multibyte? ::cat [{:keys [pattern1 pattern2]}]
  (or (multibyte? pattern1)
      (multibyte? pattern2)))

(defmethod multibyte? ::alt [{:keys [pattern1 pattern2]}]
  (or (multibyte? pattern1)
      (multibyte? pattern2)))

(defmethod multibyte? ::char-range [{:keys [upper lower]}]
  (or (multibyte? upper)
      (multibyte? lower)))

(defmethod multibyte? ::integer [i]
  {:pre [(>= (int i) 0)]}
  (> (int i) 127))

(defn cat
  "(con)catenation of patterns.

  Returns a pattern which matches the left first, and then the right."
  [a b]
  {:tag        ::cat
   :multi-byte (or (multibyte? a)
                   (multibyte? b))
   :pattern1   a
   :pattern2   b})

(defn alt
  "Alternation of patterns.

  Returns a pattern which matches either the left or the right pattern.

  May also be thought of as union between patterns."
  [a b]
  {:tag        ::alt
   :multi-byte (or (multibyte? a)
                   (multibyte? b))
   :pattern1   a
   :pattern2   b})

;; We're gonna support all character values, being positive integers between `0` and
;; `Character/MAX_CODE_POINT`. A character set then is a set of characters which we'll allow to
;; match at a given terminal. The traditional encoding is lists of ranges (or singletons), which
;; represent either included or excluded ranges of the codepoint or byte space.
;;
;; What we want to use (or emit) then is the most compact set representation for all characters
;; which will match.
(defn char-range
  "Implementation detail of character sets.

  Defines a character range with an upper and lower bound.

  The bounds may be equal if only one char is matched."
  [lower upper]
  {:pre [(or (integer? lower) (char? lower))
         (or (integer? upper) (char? upper))]}
  (let [upper* (max (int upper) (int lower))
        lower* (min (int upper) (int lower))]
    {:tag        ::char-range
     :multi-byte (or (multibyte? upper)
                     (multibyte? lower))
     :upper      upper*
     :lower      lower*}))

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
           :multi-byte (or (multibyte? l-upper)
                           (multibyte? r-lower))
           :upper      l-upper
           :lower      r-lower}

          (> l-lower r-upper)
          ;; L and R are disjoint, non-continuous. Build a set.
          {:tag        ::char-set
           :multi-byte (or (multibyte? l)
                           (multibyte? r))
           :ranges     [l r]}

          ;; There is no other possible case because for l-upper to be LESS than r-lower, either r
          ;; is invalid or r is strictly greater than l in which case our sorting invariant is
          ;; violated.
          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

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
                  (char-range-union a b))))))

(defn char-range-difference
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
           :multi-byte (or (multibyte? l)
                           (multibyte? r))
           :ranges     [(char-range l-upper (inc r-upper))
                        (char-range (dec r-lower) l-lower)]}

          :else
          (throw (IllegalStateException. "Entered theoretically impossible state!")))))

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
                  (if (< (:upper e) (:lower l))
                    (reduced l)
                    (char-range-difference l e))))
               l)))

(def ANY-CHAR-RANGE
  "Matches any character whatsoever."
  (char-range 0 Character/MAX_CODE_POINT))

(def NOT-NULL-CHAR-RANGE
  "Matches any character BUT the NULL (0) character."
  (char-range 1 Character/MAX_CODE_POINT))

(defn group
  "A group wrapping another pattern.

  Used lexically for concatenations to be terminated."
  [a]
  {:tag        ::group
   :multi-byte (multibyte? a)
   :pattern    a})

(defmulti render
  "Render a regexp tree to a legal Java regex string"
  #'tag-dx)

(defmethod render ::alt [{:keys [pattern1 pattern2]}]
  (str (render pattern1) "|" (render pattern2)))

(defmethod render ::integer [i]
  (str (clojure.core/char (int i)))
  #_(if (or (Character/isISOControl ^int (int i))
            (> i 127))
      (format "\\x%X" (int i))
      ))

(defmethod render ::char-range [{:keys [upper lower]}]
  (if (= upper lower)
    (render upper)
    (format "[%s-%s]" (render lower) (render upper))))

(defmethod render ::char-set [{:keys [ranges]}]
  (let [ranges (mapcat char-set* ranges)]
    (->> ranges
         (map render)
         (apply str)
         (format "[%s]"))))
