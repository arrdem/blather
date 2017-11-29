(ns irregular.imp
  "Implementation bits and bats.")

(defn tag-dx
  "Dispatch by :tag, :type, :ingeger for chars and ints,
  otherwise :default."
  [tagged-or-raw]
  (cond (map? tagged-or-raw)
        (:type tagged-or-raw
               (:tag tagged-or-raw :default))

        (or (char? tagged-or-raw)
            (integer? tagged-or-raw))
        :integer

        :else
        :default))

(defn pairwise-dx
  "A dispatching function for pairs of records."
  [x y]
  [(tag-dx x) (tag-dx y)])
