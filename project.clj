(defproject Madhava "0.3.0"
  :description "automatic differentiation for partial differential equations"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/data.int-map "0.2.4"]
                 [com.rpl/specter "1.0.1"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [criterium "0.4.4"]]
  :main ^:skip-aot Madhava.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
