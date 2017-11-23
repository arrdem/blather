(ns me.arrdem.irregular.optimize
  "Regex pattern minimization(s)."
  (:require [me.arrdem.irregular.imp :refer [tag-dx]]
            [me.arrdem.irregular.char-sets :as chars]
            [me.arrdem.irregular.combinators :as comb]))

;; Alt really is the thing that simplifies.
;;
;; Cat only simplifies when its components (or some prefix thereof) simplify.
;; 
;; Groups explicitly prevent simplification.

(defn optimize
  "Returns an equivalent but minimized grammar"
  [grammar]
  ;; FIXME
  grammar)
