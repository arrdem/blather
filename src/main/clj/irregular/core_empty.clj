;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)

(derive! #'h ::empty ::any)
(derive! #'h ::not-empty ::any)

(defn ->empty
  "Constructor. Returns a node representing the empty character set. Used as a common bottom."
  [& args]
  {:tag ::empty})

(defn empty? [{:keys [tag]}]
  (= tag ::empty))

(defmethod multibyte? ::empty [_]
  false)

(defmethod score ::empty [_] 1)

;; Have to have this because preference order
(defmethod union* [::empty ::empty] [a b] a)
(defmethod-commutative union* [::empty ::not-empty] [a b] b)

(defmethod intersection* [::empty ::empty] [a b] a)
(defmethod-commutative intersection* [::empty ::not-empty] [a b] a)

(defmethod intersects? [::empty ::empty] [a b] false)
(defmethod-commutative intersects? [::empty ::not-empty] [a b] false)

(defmethod subtraction* [::empty ::empty] [a b] a)
;; NOTE THAT SUBTRACTION IS NOT COMMUTATIVE
(defmethod subtraction* [::empty ::not-empty] [a b] a)
(defmethod subtraction* [::not-empty ::empty] [a b] a)
