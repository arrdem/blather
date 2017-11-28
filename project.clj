(defproject me.arrdem/blather "0.0.0-SNAPSHOT"
  :description "Blather - a toolkit for manipulating BNF variants."
  :url "http://github.com/arrdem/blather"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "LATEST"]
                 [instaparse/instaparse "1.4.8"]
                 [me.arrdem/guten-tag "LATEST"]]
  :source-paths ["src/main/clj"]
  :resource-paths ["src/main/resources"]
  :test-paths ["src/test/clj" "src/test/resources"]
  :profiles {:dev {:source-paths   ["src/dev/clj"]
                   :resource-paths ["src/dev/resources"]}})
