(defproject madhava "0.5.5-SNAPSHOT"
  :description "automatic differentiation for partial differential equations"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [io.rkn/core.async "0.1.0-SNAPSHOT"]
                 [org.clojure/data.int-map "0.2.4"]
                 [primitive-math "0.1.6"]
                 [clj-tuple "0.2.2"]
                 [local/data.avl "0.0.18-SNAPSHOT"]
                 [com.rpl/specter "1.0.3"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.nrepl "0.2.12"]
                                  [criterium "0.4.4"]]}
             :uberjar {:aot :all}}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repository")))}
  :main madhava
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed})
