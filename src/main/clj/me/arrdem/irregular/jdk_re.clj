(ns me.arrdem.irregular.jdk-re
  "Compiles an irregular regex IR down to a JDK legal regex string."
  (:require [me.arrdem.irregular :as i :refer [tag-dx]]
            [me.arrdem.irregular.char-sets :as s :refer [char-set*]]))

(defmulti render
  "Render a regexp tree to a legal Java regex string"
  #'tag-dx)

(defmethod render ::i/alt [{:keys [pattern1 pattern2]}]
  (str (render pattern1) "|" (render pattern2)))

(defmethod render ::i/integer [i]
  (if (or (Character/isISOControl ^int (int i))
          (> i 127))
    (format "\\u%04X" (int i))
    (str (clojure.core/char (int i)))))

(defmethod render ::s/char-range [{:keys [upper lower]}]
  (if (= upper lower)
    (render upper)
    (format "[%s-%s]" (render lower) (render upper))))

(defmethod render ::s/char-set [{:keys [ranges]}]
  (let [ranges (mapcat char-set* ranges)]
    (->> ranges
         (map render)
         (apply str)
         (format "[%s]"))))

(defmethod render ::i/cat [{:keys [pattern1 pattern2]}]
  (str (render pattern1) (render pattern2)))

(defmethod render ::i/group [{:keys [pattern]}]
  (format "(%s)" (render pattern)))

