# Home assignment #2

## Parsing ASN.1 Data

### Introduction

ASN.1 is based on encoding that optimizes for space, some interesting links which could come in handy:

1. [MSDN Documentation] (https://msdn.microsoft.com/en-us/library/windows/desktop/bb540809(v=vs.85).aspx)
2. [A layman's guide] (http://luca.ntop.org/Teaching/Appunti/asn1.html)
3. [Java ByteBuffer documentation] (https://docs.oracle.com/javase/7/docs/api/java/nio/ByteBuffer.html)

Additionally, the openssl tool might come in handy to compare results:

> cat /path/to/key.pem | openssl asn1parse

### Objective

Write an ASN.1 parser that is sufficient to handle private keys in the EC and RSA format.

### Assignment details

No dependencies should be pulled-in by the project. An invocation of 
> lein run <path/to/key> 
should produce on standard output the data representation of the parsed contents. An output consisting only of using clojure.pprint/pprint on structured data would be perfectly fine.

To simplify the process the following namespace can be used as a starting point:

`
(ns asn1.parser
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [clojure.pprint  :refer [pprint]])
  (:import java.io.RandomAccessFile
           java.nio.ByteBuffer
           javax.xml.bind.DatatypeConverter))

(defn base64-extract
  [path]
  (reduce str "" (remove #(str/starts-with? % "----") (line-seq (io/reader path)))))

(defn base64-bytes
  [path]
  (let [b64-str ^String (base64-extract path)]
    (DatatypeConverter/parseBase64Binary b64-str)))

(defn base64-buffer
  [path]
  (ByteBuffer/wrap (base64-bytes path)))

(defn parse-asn1
  [bb]
  ::nothing-parsed)
  
(defn -main [& args]
  (if-let [key-path (first args)]
    (pprint (parse-asn1 (base64-buffer key-path)))
	(binding [*out* *err*]
	  (println "no path given")
	  (System/exit 1))))
`
