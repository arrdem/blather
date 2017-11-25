(ns user)

(defn match [pattern string]
  (let [[string & groups] (re-find pattern string)]
    {:matched-text string
     :groups       (into (sorted-map) vector (range 1 Long/MAX_VALUE) groups)}))
