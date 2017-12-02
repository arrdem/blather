;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)

(derive! #'h ::collation ::not-empty)

;; Character sets may provide collating symbol groups.
(defn ->collation
  "Constructor.

  Returns a collation class within a language and character set.

  Collation classes cannot be evaluated particularly meaningfully for
  anything but equality."
  [name]
  {:tag  ::collation
   :name name})

(defn collation?
  "Predicate. Returns `true` iff given a collation class."
  [{:keys [tag]}]
  (= tag ::collation))

;; FIXME: we can't really tell
(defmethod multibyte? ::collation [_]
  true)

(defmethod score ::collation [_]
  1)

;; Take the default union
;; Take the default intersection
;; Take the default subtraction
