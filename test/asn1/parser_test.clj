(ns asn1.parser
  (:require [clojure.test :refer :all]
            [asn1.parser :refer :all]))

(deftest parse-type-test
  (testing "Should return type :integer when first byte is 0x02"
    (let [asn1-stream [(byte 2) (byte 1) (byte 1)]]
      (is (= {:type :integer
              :offset 0
              :length 1}
             (parse-type asn1-stream 0)))))
  (testing "Should return type :sequence when first byte is 0x30"
    (let [asn1-stream [(byte 0x30) (byte 1) (byte 1)]]
      (is (= {:type :sequence
              :offset 0
              :length 1}
             (parse-type asn1-stream 0)))))
  (testing "Should return type :optional when first byte is 0xA0"
    (let [asn1-stream [(byte -96) (byte 1) (byte 1)]]
      (is (= {:type :context-specific
              :order 0
              :offset 0
              :length 1}
             (parse-type asn1-stream 0))))))

(deftest parse-length-test
  (testing "Should read simple length field"
    (let [asn1-stream [(byte 2) (byte 1) (byte 1)]]
      (is (= {:value 1
              :offset 1
              :length 1}
             (parse-length asn1-stream 1)))))
  (testing "Should read complex length field"
    (let [asn1-stream [(byte 2) (byte -126) (byte 19) (byte 70)]]
      (is (= {:value 4934
              :offset 1
              :length 3}
             (parse-length asn1-stream 1))))
    (let [asn1-stream [(byte 2) (byte -126) (byte 1) (byte 1)]]
      (is (= {:value 257
              :offset 1
              :length 3}
             (parse-length asn1-stream 1))))))

(deftest parse-number-test
  (testing "Should read value stored in several bytes"
    (let [asn1-stream [(byte 3) (byte 1) (byte 1) (byte 5)]]
      (is (= 257
             (parse-number asn1-stream 1 3)))))
  (testing "Should return 2-complement value when read value is negative"
    (let [asn1-stream [(byte 3) (byte -127)]]
      (is (= 129
             (parse-number asn1-stream 1 2))))))

(deftest parse-integer-value-test
  (testing "Should read simple integer value"
    (let [asn1-stream [(byte 2) (byte 1) (byte 1) (byte 1) (byte 5)]]
      (is (= {:type :integer
              :value 1
              :offset 2
              :length 1}
             (parse-integer-value asn1-stream 2 1)))))
  (testing "Should read complex integer value"
    (let [asn1-stream (vec
                       (concat
                        [(byte 2)]
                        [(byte -126) (byte 1) (byte 1)]
                        (cons 1 (repeat 256 (byte 0)))))
          parsed-value (parse-integer-value asn1-stream 4 257)]
      (is (= :integer (:type parsed-value)))
      (is (= 4 (:offset parsed-value)))
      (is (= 257 (:length parsed-value)))
      (is (= "100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
             (.toString (:value parsed-value) 16))))))

(deftest parse-seq-test
  (testing "Should parse all elements when reading a sequence value"
    (let [asn1-stream [(byte 48) (byte 6) (byte 2) (byte 1) (byte 1) (byte 2) (byte 1) (byte 1) (byte 1)]]
      (is (= {:type :sequence
              :value [{:type :integer :length 1 :value 1 :offset 4}
                      {:type :integer :length 1 :value 1 :offset 7}]
              :offset 2
              :length 6}
             (parse-seq-value asn1-stream 2 6))))))

(deftest parse-octet-string-value-test
  (testing "Should read octet string as a hex string"
    (let [asn1-stream [(byte 4) (byte 10) (byte 30) (byte 8) (byte 0) (byte 85) (byte 0) (byte 115) (byte 0) (byte 101) (byte 0) (byte 114)]]
      (is (= {:type :octet-string
              :value "[HEX DUMP]:1E 08 00 55 00 73 00 65 00 72"
              :offset 2
              :length 10}
             (parse-octet-string-value asn1-stream 2 10))))))

(deftest parse-bit-string-value-test
  (testing "Should read bit string as a string of bits"
    (let [asn1-stream [(byte 3) (byte 3) (byte 4) (byte 87) (byte -48)]]
      (is (= {:type :bit-string
              :value "010101111101"
              :offset 2
              :length 3}
             (parse-bit-string-value asn1-stream 2 3))))))

(deftest parse-object-id-value-test
  (testing "Should parse object-id as text joined by '.'"
    (let [asn1-stream [(byte 6) (byte 9) (byte 43) (byte 6) (byte 1) (byte 4) (byte 1) (byte -126) (byte 55) (byte 21) (byte 20)]]
      (is (= {:type :object-id
              :value "1.3.6.1.4.1.311.21.20"
              :offset 2 :length 9}
             (parse-object-id-value asn1-stream 2 9))))
    (let [asn1-stream [(byte 6) (byte 7) (byte 42) (byte -122) (byte 72) (byte -50) (byte 61) (byte 1) (byte 1)]]
      (is (= {:type :object-id
              :value "1.2.840.10045.1.1"
              :offset 2
              :length 7}
             (parse-object-id-value asn1-stream 2 7))))))
