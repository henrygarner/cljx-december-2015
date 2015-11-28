(defproject example "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [compojure "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [ring/ring-json "0.4.0"]
                 [http-kit "2.1.18"]
                 [tesser.core "1.0.1"]
                 [tesser.math "1.0.1"]
                 [criterium "0.4.3"]
                 [incanter "1.5.6"]
                 [org.hdrhistogram/HdrHistogram "2.1.2"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler example.handler/app
         :nrepl {:start? true}})
