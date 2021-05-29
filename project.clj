(defproject slackat "1.0.0"
  :description "slack scheduler"
  :url "https://github.com/jaemk/slackat"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.logging "1.1.0"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/core.async "1.3.618"]
                 [nrepl "0.8.3"]
                 [com.taoensso/timbre "5.1.2"]
                 [org.slf4j/slf4j-simple "1.7.28"]
                 [migratus "1.2.3"]
                 [aleph "0.4.7-alpha7"]
                 [manifold "0.1.9-alpha4"]
                 [byte-streams "0.2.5-alpha2"]
                 [byte-transforms "0.1.5-alpha1"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/core.cache "1.0.207"]
                 [ring/ring-core "1.9.3"]
                 [compojure "1.6.2"]
                 [buddy/buddy-core "1.10.1"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [hikari-cp "2.13.0"]
                 [org.postgresql/postgresql "42.2.20.jre7"]
                 [honeysql "1.0.461"]
                 [nilenso/honeysql-postgres "0.4.112"]
                 [org.ocpsoft.prettytime/prettytime-nlp "5.0.1.Final"]
                 [cheshire "5.10.0"]]
  :main ^:skip-aot slackat.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-binplus "0.6.6"]
                             [lein-midje "3.2.2"]]
                   :dependencies [[midje "1.10.3"]]
                   :source-paths ["dev"]
                   :main user}}
  :bin {:name "slackat"
        :bin-path "out"
        :jvm-opts ["-server" "-Dfile.encoding=utf-8" "$JVM_OPTS"]
        :custom-preamble "#!/bin/sh\nexec java {{{jvm-opts}}} -jar $0 \"$@\"\n"})
