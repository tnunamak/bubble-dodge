(defproject tinygame "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-lanterna "0.9.4"]
                 [overtone/at-at "1.2.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot tinygame.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
