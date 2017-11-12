(defproject madhava "0.5.3"
  :description "automatic differentiation for partial differential equations"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/data.avl "0.0.17"] ;; DELETE!
                 [com.rpl/specter "1.0.3"]
                 [org.clojure/tools.analyzer "0.6.9"]
                 [org.clojure/tools.analyzer.jvm "0.7.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [criterium "0.4.4"]]
  :main madhava.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
