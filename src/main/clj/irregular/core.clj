(ns irregular.core
  "Regular expressions.

  As data structures.

  It's a rite of passage I guess."
  (:refer-clojure :exclude [empty? class?]))

(defn tag-dx
  "Dispatch by :tag, :type, :ingeger for chars and ints,
  otherwise :default."
  [tagged-or-raw]
  (cond (map? tagged-or-raw)
        (:type tagged-or-raw
               (:tag tagged-or-raw :default))

        (char? tagged-or-raw)
        ::character

        (string? tagged-or-raw)
        ::string

        (keyword? tagged-or-raw)
        tagged-or-raw

        :default
        (type tagged-or-raw)))

(defn pairwise-dx
  "A dispatching function for pairs of records."
  [x y]
  [(tag-dx x) (tag-dx y)])

(defn derive!
  "Although hierarchies are mostly used by indirection as
  vars (`defmulti` supports the `:hierarchy` argument for using
  hierarchies other than the default global one), and
  although `(derive child parent)` implicitly side-effects the global
  hierarchy, `(derive h child parent)` is a pure function of a
  hierarchy as a map structure which doesn't behave nicely when given
  a var.

  Wrap it up with an alter-var-root so it makes sense."
  [h child parent]
  {:pre [(var? h)]}
  (alter-var-root h derive child parent))

(def h
  "NOT FOR PUBLIC CONSUMPTION.

  The hierarchy used to structure irregular ASTs."
  (make-hierarchy))

;; The dispatch hierarchy is as follows:
;; ::any
;;  - ::empty
;;  - ::not-empty
;;    - <all other tags must be children of not-empty>
;;
;; This allows us to coherently special case arithmetic with empty, and only have one empty in the
;; algebra as everything else can normalize to the sentinel empty.

(defmulti multibyte?
  "Determines whether a given value requires multibyte pattern matching."
  #'tag-dx
  :hierarchy #'h)

(defmulti score
  "Attempts to score the simplicity of an expression by counting terms.

  Lower scores are simpler. Scores of products must accumulate.

  Used when attempting to compute simplifications to determine when an
  equivalent computation is no simpler than original computation."
  #'tag-dx
  :hierarchy #'h)

(defmulti children
  "For any given node, returns the sequence if its sub-terms."
  #'tag-dx
  :hierarchy #'h)

;; Forward declaration to help out writing the component fragments
(declare ->empty ->union ->subtraction ->intersection ->range)

;; Union
;;------------------------------------------------
(defmulti union*
  "Extension point.

  Implements union between a pair of character set values of possibly
  different types.

  Note to the implementer: Most character set structures cannot be
  unioned to concrete values. Consequently, the default behavior for
  this method should be to produce a union structure such as

    {:tag ::union
     :terms #{...}}

  The union of unions is the union (possibly but not necessarily
  simplified), and for some specific representations unions within the
  representation may reduce."
  #'pairwise-dx
  :hierarchy #'h)

(defn union
  "Union on character sets.

  This method should be preferred to `#'union*` in client code.

  By default, produces an empty union structure."
  [& sets]
  (reduce union* (->empty) sets))

;; Intersection
;;------------------------------------------------
(defmulti intersection*
  "Extension point

  Implements intersection between character set values of possibly
  different types.

  Note to the implementer: Most character set structures cannot be
  intersected by value. Consequently, the default behavior for this
  function should be to produce a symbolic intersection structure such
  as

    {:tag ::intersection
     :a   <term>
     :bs  #{<term>}}

  Note that a set is usable since intersection is order-independent."
  #'pairwise-dx
  :hierarchy #'h)

(defn intersection
  "Intersection on character sets.

  This method should be preferred to `#'intersection*` in client
  code."
  [set & sets]
  (reduce intersection* set sets))

(defmulti intersects?
  "Predicate. Extension point.

  Returns `true` if and only if the two argument structures can be
  determined to intersect. If there is not a trivial intersection,
  returns `false`.

  Note to the implementer: False negatives are acceptable. False
  positives are not."
  #'pairwise-dx
  :hierarchy #'h)

;; Subtraction
;;------------------------------------------------
(defmulti subtraction*
  "Extension point.

  Implements subtraction between character set types.

  Note to the implementer: Most character set structures cannot be
  subtracted by value. Consequently, the default behavior for this
  function should be to produce a symbolic structure such as

    {:tag ::subtraction
     :a   <term>
     :bs  #{<term>}}

  Where :a is the value to be subtracted from, and :bs is the values
  to subtract."
  #'pairwise-dx
  :hierarchy #'h)

(defn subtraction
  "Subtraction on character sets.

  This method should be preferred to `#'subtraction*` in client code."
  [set & sets]
  (reduce subtraction* set sets))

;; A helper since I'm gonna write lots of binary ops.
(defn- reversev [seq]
  (vec (reverse seq)))

(defmacro ^:private defmethod-commutative
  "Helper macro for defining multimethod implementations where the method is BINARY and COMMUTATIVE.

  Cheats by only defining the explicitly listed dispatch constant with
  the listed function body, and defining the other by self-calling the
  multimethod.."
  [method constant args & fn-body]
  {:pre [(every? symbol? args)]}
  `(do (defmethod ~method ~constant ~args ~@fn-body)
       (defmethod ~method ~(reversev constant) ~(reversev args) (~method ~@args))))

;; Special terminals

(def EOF
  "End Of File (input)"
  ::eof)

(defmethod multibyte? ::eof [_] false)

(def SOF
  "Start Of File (input)"
  ::sof)

(defmethod multibyte? ::sof [_] false)

;; Load up the fragmented implementation because multimethods are imperative.
;;--------------------------------------------------------------------------------------------------

(load "core_empty")
(load "core_union")
(load "core_subtraction")
(load "core_intersection")
(load "core_equiv")
(load "core_collation")
(load "core_class")
(load "core_char")
(load "core_range")
