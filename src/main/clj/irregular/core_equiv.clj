;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)

(derive! #'h ::equivalence ::not-empty)

;; Character sets support equivalence classes - groups of characters which may be considered to be
;; equal for various reasons.
(defn ->equiv
  "Constructor.

  Returns a named equivalence class in a given character set &
  language.

  Equivalence classes cannot be evaluated particularly meaningfully
  for much of anything but equality."
  [name]
  {:tag  ::equivalence
   :name name})

(defn equiv?
  "Predicate. Returns `true` iff given an equivalence class."
  [{:keys [tag]}]
  (= tag ::equivalence))

;; FIXME: we can't really tell
(defmethod multibyte? ::equivalence [_]
  true)

(defmethod score ::equivalence [_]
  1)

;; Take the default union
;; Take the default intersection
;; Take the default subtraction
