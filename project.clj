(defproject football "0.0.1-SNAPSHOT"
  :description "A football simulation."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [korma "0.3.0-RC5"]
                 [org.apache.commons/commons-math3 "3.1"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
  :main football.simapp)
