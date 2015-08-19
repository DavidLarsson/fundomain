(ns fundomain.cnum-test
  (:require [clojure.core :as c]
            [clojure.test :refer :all]
            [fundomain.cnum :as cnum]
            [fundomain.util :as u]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(defspec reabs-is-idempotent
  100
  (prop/for-all [n gen/ratio]
                (c/= (cnum/reabs n)
                     (cnum/reabs (cnum/reabs n)))))

(defspec strz-creates-a-proper-complex-string
  100
  (prop/for-all [m (gen/hash-map :real gen/ratio
                                 :imag gen/ratio)]
                (u/not-nil? (re-find #"\d*([+-]i\d*)?"
                                     (cnum/strz m)))))

(defspec conjugation-is-a-reflection
  100
  (prop/for-all [m (gen/hash-map :real gen/ratio
                                 :imag gen/ratio)]
                (c/= m
                     (cnum/conjugate (cnum/conjugate m)))))
