(ns fundomain.cnum
  (:refer-clojure :exclude [+ - / * abs]))

(defstruct cnum :real :imag)

(defn z
  "Construct a cnum-struct, by supplying the real and imaginary parts."
  ([real]
   (struct cnum real 0))
  ([real imaginary]
   (struct cnum real imaginary)))

(defn complex?
  "Test for complexity."
  [c]
  (and (map? c)
       (contains? c :real)
       (contains? c :imag)))

; Unlike Math/abs, also works with fractions.
(defn reabs
  "The absolute value of a real number."
  [n]
  {:pre [(number? n)]}
  (if (< n 0)
    (clojure.core/- n)
    n))

(defn abs2
  "The square of the absolute value of a complex number."
  [c]
  {:pre [(complex? c)]}
  (clojure.core/+ (clojure.core/* (:real c)
                                  (:real c))
                  (clojure.core/* (:imag c)
                                  (:imag c))))

(defn abs
  "The absolute value of a complex number."
  [c]
  {:pre [(complex? c)]}
  (Math/sqrt (abs2 c)))

(defn strz
  "Create a string representation of a complex number."
  [c]
  {:pre [(complex? c)]}
  (let [op (cond (clojure.core/= 0 (:imag c)) ""
                 (< (:imag c) 0) "-"
                 :else "+")]
    (str (:real c)
                 op
                 (when (not (clojure.core/= 0 (:imag c)))
                   (str "i" (reabs (:imag c)))))))
(defn +
  "Add complex numbers."
  ([] (struct cnum 0 0))
  ([c1]
   {:pre [(complex? c1)]}
   c1)
  ([c1 c2]
   {:pre [(complex? c1)
          (complex? c2)]}
   (struct cnum
           (clojure.core/+ (:real c1)
                           (:real c2))
           (clojure.core/+ (:imag c1)
                           (:imag c2))))
  ([c1 c2 & cs]
   {:pre [(complex? c1)
          (complex? c2)
          (every? complex? cs)]}
   (reduce +
           (+ c1 c2)
           cs)))

(defn -
  "Subtract complex numbers."
  ([] (struct cnum 0 0))
  ([c1]
   {:pre [(complex? c1)]}
   (struct cnum
           (clojure.core/- (:real c1))
           (clojure.core/- (:imag c1))))
  ([c1 c2]
   {:pre [(complex? c1)
          (complex? c2)]}
   (struct cnum
           (clojure.core/- (:real c1)
                           (:real c2))
           (clojure.core/- (:imag c1)
                           (:imag c2))))
  ([c1 c2 & cs]
   {:pre [(complex? c1)
          (complex? c2)
          (every? complex? cs)]}
   (reduce -
           (- c1 c2)
           cs)))

(defn *
  "Multiply complex numbers."
  ([] (struct cnum 1 0))
  ([c1]
   {:pre [(complex? c1)]}
   c1)
  ([c1 c2]
   {:pre [(complex? c1)
          (complex? c2)]}
   (struct cnum
           (clojure.core/- (clojure.core/* (:real c1)
                                           (:real c2))
                           (clojure.core/* (:imag c1)
                                           (:imag c2)))
           (clojure.core/+ (clojure.core/* (:real c1)
                                           (:imag c2))
                           (clojure.core/* (:imag c1)
                                           (:real c2)))))
  ([c1 c2 & cs]
   {:pre [(complex? c1)
          (complex? c2)
          (every? complex? cs)]}
   (reduce *
           (* c1 c2)
           cs)))

(defn /
  "Divide complex numbers."
  ([] (struct cnum 1 0))
  ([c1]
   {:pre [(complex? c1)]}
   (struct cnum
           (clojure.core// (:real c1)
                           (abs2 c1))
           (clojure.core// (clojure.core/- (:imag c1))
                           (abs2 c1))))
  ([c1 c2]
   {:pre [(complex? c1)
          (complex? c2)]}
   (* c1 (/ c2)))
  ([c1 c2 & cs]
   {:pre [(complex? c1)
          (complex? c2)
          (every? complex? cs)]}
   (reduce /
           (/ c1 c2)
           cs)))




(defn conjugate
  "Conjugate a complex number, a+ib => a-ib."
  [c]
  {:pre [(complex? c)]}
  (struct cnum
          (:real c)
          (clojure.core/- (:imag c))))



(defn normalize
  "Normalize a complex number, i.e., divide by its absolute value."
  [c]
  {:pre [(complex? c)]}
  (/ c (struct cnum (abs c) 0)))

(defn arg
  "The principal argument of a complex number (-Pi<arg(c)<=Pi)."
  [c]
  {:pre [(complex? c)]}
  (let [d (normalize c)]
    (if (>= (:imag c) 0)
      (Math/acos (:real d))
      (clojure.core/+ (clojure.core/- Math/PI)
                      (Math/acos (clojure.core/- (:real d)))))))
