(ns me.arrdem.blather.grammars
  "The internal representation of a Blather grammar.")

(defn rule
  "Constructor. Returns a rule node referencing the named rule."
  [rule-name]
  {:tag  ::rule
   :name rule-name})
