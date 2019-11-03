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

(defn raw-hex [bytes]
  (str/join " " (map #(format "%02X" %) bytes)))

(def asn1-types {2 :integer
                 3 :bit-string
                 4 :octet-string
                 5 :null
                 6 :object-id
                 16 :sequence
                 17 :set})

(defn parse-type [asn1-stream offset]
  (let [first-byte (asn1-stream offset)
        bit-7 (bit-test first-byte 7)
        bit-6 (bit-test first-byte 6)
        six-bits (bit-and first-byte 2r00011111)
        next (inc offset)]
    (if (and bit-7 (not bit-6))
      {:type :context-specific
       :order six-bits
       :offset offset
       :length 1}
      {:type (asn1-types six-bits)
       :offset offset
       :length 1})))

(defn parse-number [asn1-stream start end]
  (let [readed (biginteger (byte-array (subvec asn1-stream start end)))]
    (if (neg? readed)
      (.add readed (biginteger (* 256 (- end start))))
      readed)))

(defn complex-length? [first-byte]
  (bit-test first-byte 7))

(defn parse-complex-length [asn1-stream offset]
  (let [first-byte (asn1-stream offset)
        num-bytes (bit-and first-byte 2r01111111)
        beginning (inc offset)
        end (+ beginning num-bytes)]
    {:value (parse-number asn1-stream beginning end)
     :offset offset
     :length (inc num-bytes)}))

(defn parse-length [asn1-stream offset]
  (let [first-byte (asn1-stream offset)]
    (if (complex-length? first-byte)
      (parse-complex-length asn1-stream offset)
      {:value (biginteger first-byte)
       :offset offset
       :length 1})))

(defn parse-integer-value [asn1-stream offset length]
  (let [beginning offset
        end (+ offset length)
        value (parse-number asn1-stream beginning end)]
    {:type :integer
     :value value
     :offset offset
     :length length}))

(defn parse-octet-string-value [asn1-stream offset length]
  (let [end (+ offset length)
        value (subvec asn1-stream offset end)
        hex-value (str "[HEX DUMP]:" (raw-hex value))]
    {:type :octet-string
     :value hex-value
    :offset offset
    :length length}))

(defn add-left-0-padding [bit-string n]
  (reduce #(str %2 %1) bit-string (take n (repeat 0))))

(defn parse-bit-string-value [asn1-stream offset length]
  (let [end (+ offset length)
        value-offset (inc offset)
        pad-length (parse-number asn1-stream offset value-offset)
        value-with-padding (parse-number asn1-stream value-offset end)
        value-without-padding (.shiftRight value-with-padding pad-length)
        bit-string (.toString value-without-padding 2)
        required-length (- (* 8 (dec length)) pad-length)
        left-padding-to-add (- required-length (.length bit-string))
        bit-string-padded (add-left-0-padding bit-string left-padding-to-add)]
    {:type :bit-string
     :value bit-string-padded
     :offset offset
     :length length}))

(defn parse-null-value [asn1-stream offset length]
  {:type :null
   :value "NULL"
   :offset offset
   :length length})

(defn complex-elem? [elem]
  (bit-test elem 7))

(defn conj-oid-elem [coll elem]
  (if (complex-elem? elem)
    (conj coll [elem])
    (conj coll elem)))

(defn group-complex-oid-elements [elements]
  (reduce (fn [acc elem]
            (let [prev-elem (last acc)
                  prev-elem-vec? (vector? prev-elem)]
              (if prev-elem-vec?
                (if (complex-elem? (last prev-elem))
                  (assoc acc (dec (count acc)) (conj prev-elem elem))
                  (conj-oid-elem acc elem))
                (conj-oid-elem acc elem))))
          []
          elements))

(defn calculate-oid-value [acc elem]
  (let [reversed (reverse elem)]
    (loop [resul 0
           byte-order 0
           first-byte (first reversed)
           more-bytes (rest reversed)]
      (if first-byte
        (let [seven-bits (bit-and first-byte 2r1111111)
              exp (dec (* 8 byte-order))
              decimal-value (int (* (Math/pow 2 (if (neg? exp) 0 exp)) seven-bits))]
          (recur
           (+ resul decimal-value)
           (inc byte-order)
           (first more-bytes)
           (rest more-bytes)))
        (conj acc resul)))))

(defn process-grouped-elements [elements]
  (reduce (fn [acc elem]
            (if (vector? elem)
              (calculate-oid-value acc elem)
              (conj acc elem)))
          []
          elements))

(defn join-oid-values [v1 v2 more-values]
  (str/join "." (conj [] v1 v2 (str/join "." more-values))))

(defn parse-object-id-value [asn1-stream offset length]
  (let [end (+ offset length)
        first-byte (asn1-stream offset)
        first-value (quot first-byte 40)
        second-value (mod first-byte 40)
        remaining-elements (subvec asn1-stream (inc offset) end)
        grouped-elements (group-complex-oid-elements remaining-elements)
        more-values (process-grouped-elements grouped-elements)
        joined-values (join-oid-values first-value second-value more-values)]
    {:type :object-id
     :value joined-values
     :offset offset
     :length length}))

(declare parse-next)

(defn parse-context-specific-value [asn1-stream offset length type]
  {:type :context-specific
   :order (:order type)
   :value (parse-next asn1-stream offset)
   :offset offset
   :length length})

(defn parse-seq-value [asn1-stream offset length]
  (let [limit (+ offset length)
        initial {:type :sequence
                 :value []
                 :offset offset
                 :length length}]
    (loop [value-offset offset
           resul initial]
      (if (< value-offset limit)
        (let [next-value (parse-next asn1-stream value-offset)]
          (recur (+ (:offset next-value) (:length next-value))
                 (update resul :value conj next-value)))
        resul))))

(defn parse-next [asn1-stream offset]
  (let [type (parse-type asn1-stream offset)
        length (parse-length asn1-stream (+ offset (:length type)))
        length-value (:value length)
        value-offset (+ (:offset length) (:length length))
        type-value (:type type)]
    (case type-value
      :sequence (parse-seq-value asn1-stream value-offset length-value)
      :integer (parse-integer-value asn1-stream value-offset length-value)
      :octet-string (parse-octet-string-value asn1-stream value-offset length-value)
      :bit-string (parse-bit-string-value asn1-stream value-offset length-value)
      :object-id (parse-object-id-value asn1-stream value-offset length-value)
      :null (parse-null-value asn1-stream value-offset length-value)
      :context-specific (parse-context-specific-value asn1-stream value-offset length-value type))))

(defn read-buffer [bb]
  (let [len (.remaining bb)
        dest (byte-array len)]
    (do
      (.get bb dest)
      (vec dest))))

(defn parse-asn1
  [bb]
  (let [asn1-stream (read-buffer bb)]
    {:hex (raw-hex asn1-stream)
     :parsed (parse-next asn1-stream 0)}))

(defn -main [& args]
  (if-let [key-path (first args)]
    (pprint (parse-asn1 (base64-buffer key-path)))
    (binding [*out* *err*]
      (println "no path given")
      (System/exit 1))))

