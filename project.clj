(defproject cogent "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :plugins [[lein-eftest "0.6.0"]]
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns cogent.core}

  :eftest {:multithread? false
           :report eftest.report.pretty/report})
