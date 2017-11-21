(ns me.arrdem.irregular.imp)

(defn tag-dx [tagged-or-raw]
  (cond (map? tagged-or-raw)
        (:tag tagged-or-raw :default)

        (or (char? tagged-or-raw)
            (integer? tagged-or-raw))
        :integer

        :else
        :default))

