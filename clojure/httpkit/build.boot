(set-env!
  :source-paths   #{"src"}
  :dependencies '[[org.clojure/clojure "1.8.0" :scope "provided"]
                  [org.clojure/tools.logging "0.3.1"]
                  [environ "1.1.0"]
                  [compojure "1.5.1"]
                  [cheshire  "5.6.3"]
                  [http-kit "2.2.0"]])

(task-options!
  pom {:project 'websocket
       :version "0.0.1"
       :description "A test Websocket server"}
  aot {:all true}
  jar {:file "app.jar"}
  uber {:exclude #{#"(?i)^META-INF/INDEX.LIST$"
                   #"(?i)^META-INF/[^/]*\.(MF|SF|RSA|DSA)$"
                   #"(?i)^META-INF/LICENSE$"
                   #"(?i)^LICENSE$"}})

(deftask build []
  (comp
    (aot :all true)
    (uber)
    (pom)
    (jar :main 'websocket.main)
    (sift :include #{ #"^app\.jar$" })
    (target :dir #{"build"})))
