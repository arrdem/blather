(ns me.arrdem.blather.ir
  "This namespace exists to specify an internal representation of BNF
  grammars which can be transformed and rendered into other BNF
  dialects."
  (:require [clojure.core :as c]
            [clojure.set :as s]
            [me.arrdem.blather.features :as features]
            [guten-tag.core :refer [deftag]]))
