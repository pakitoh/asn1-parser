(defproject asn1 "0.1.0-SNAPSHOT"
  :description "ASN1 parser for private keys in the EC and RSA format"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot asn1.parser
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
