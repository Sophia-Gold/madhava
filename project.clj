(defproject Madhava "0.1.0"
  :description "automatic differentiation for partial differential equations"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [criterium "0.4.4"]]
  :main ^:skip-aot Madhava.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
