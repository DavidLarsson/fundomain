(defproject fundomain "0.1.0-SNAPSHOT"
  :description "An application for computing fundamental domains for certain types of Fuchsian groups."
  :url "http://github.com/DavidLarsson/fundomain"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/test.check "0.7.0"]
                 [expresso "0.2.0"]]
  :main ^:skip-aot fundomain.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
