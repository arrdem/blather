(ns irregular.char
  (:require [irregular.imp :refer [tag-dx]]))

(defmulti multibyte?
  "Determines whether a given value requires multibyte pattern matching."
  #'tag-dx)


(defmethod multibyte? :default [m]
  false)

(defmethod multibyte? :integer [i]
  {:pre [(>= (int i) 0)]}
  (> (int i) 127))
