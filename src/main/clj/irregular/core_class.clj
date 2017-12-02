;; This is NOT A NAMESPACE
;; IT IS A CODE FRAGMENT
;;
;; This is loaded as part of the irregular namespace
(in-ns 'irregular.core)

(derive! #'h ::class ::not-empty)

;; Character sets support named classes - groups of characters defined by a language and coding and
;; referred to collectively with one name.
(defn ->class
  "Constructor.

  Returns a named character class in a given character set & language.

  Character classes may but need not be evaluated particularly
  meaningfully for much of anything but equality."
  [name]
  {:tag  ::class
   :name name})

(defn class?
  "Predicate. Returns `true` iff given an equivalence class."
  [{:keys [tag]}]
  (= tag ::class))

;; FIXME: we can't really tell
(defmethod multibyte? ::class [_]
  true)

(defmethod score ::class [_]
  1)

;; Take the default union
;; Take the default intersection
;; Take the default subtraction
