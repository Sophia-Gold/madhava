(defproject madhava "0.5.2"
  :description "automatic differentiation for partial differential equations"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [io.rkn/core.async "0.1.0-SNAPSHOT"]
                 [org.clojure/data.int-map "0.2.4"]
                 [local/data.avl "0.0.18-SNAPSHOT"]
                 [com.rpl/specter "1.0.3"]
                 [criterium "0.4.4"]]
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repository")))}
  :main madhava.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
