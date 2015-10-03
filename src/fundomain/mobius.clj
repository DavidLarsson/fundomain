(ns fundomain.mobius
  (:refer-clojure :exclude [record?])
  (:require [numeric.expresso.core :as sym]
            [fundomain.cnum :refer [complex? z conjugate abs2] :as c]
            [fundomain.hyperbolic-geometry :as hyp]))

(defstruct mobius :coeff)

(defn create-mobius
  "Make sure we are dealing with a complex Möbius transformation and that it is non-singular."
  [a b c d]
  {:pre [(complex? a)
         (complex? b)
         (complex? c)
         (complex? d)
         (not (= (z 0) (c/- (c/* a d)
                            (c/* b c))))]}
  (struct mobius (vector (vector a b) (vector c d))))

(def mob-id (create-mobius (z 1) (z 0)
                           (z 0) (z 1)))

(defn determinant
  "The determinant of the matrix representation of the Möbius transformation"
  [{:keys [coeff]}]
  (let [[[a b] [c d]] coeff]
    (c/- (c/* a d)
         (c/* b c))))

(defn mobius?
  "Is a map a Möbius transformation?"
  [{:keys [coeff]}]
  (let [[[a b] [c d]] coeff]
    (and (complex? a)
         (complex? b)
         (complex? c)
         (complex? d)
         (not (= 0 (c/- (c/* a d)
                        (c/* b c)))))))

(defn apply-mobius
  "Apply the Möbius transformation to a complex number."
  [f z]
  {:pre [(complex? z)
         (mobius? f)]}
  (reduce c// (map #(c/+ (c/* (first %) z)
                            (second %)) (:coeff f))))

(defn create-mobiusf
  "Create a functional representation of the Möbius transormation accepting a complex number."
  [f]
  {:pre [(mobius? f)]}
  (partial apply-mobius f))

(defn comp-mobius
  "Compose Möbius transformations as matrices."
  [f1 f2]
  {:pre [(mobius? f1)
         (mobius? f2)]}
  (let [[[a1 b1] [c1 d1]] (:coeff f1)
        [[a2 b2] [c2 d2]] (:coeff f2)]
    (create-mobius (c/+ (c/* a1 a2)
                        (c/* b1 c2))
                   (c/+ (c/* a1 b2)
                        (c/* b1 d2))
                   (c/+ (c/* c1 a2)
                        (c/* d1 c2))
                   (c/+ (c/* c1 b2)
                        (c/* d1 d2)))))

(defn isometric-circle
  "Return a simplified symbolic expression of the isometric circle of a Möbius transformation."
  [f]
  {:pre [(mobius? f)]}
  (let [[[_ _] [c d]] (:coeff f)]
    (sym/simplify
     (sym/ex (= 1 (+ (* ~(abs2 c) (+ (* x x)
                                     (* y y)))
                     ~(abs2 d)
                     (* 2
                        ~(:real (c/* c (conjugate d)))
                        x)
                     (* 2
                        ~(:imag (c/* (conjugate c) d))
                        y)))))))

(defn disc-mobius?
  "Is the Möbius transformation part of the
  automorphism group of the hyperbolic disc model?"
  [mob]
  {:pre [(mobius? mob)]}
  (let [[[a b] [c d]] (:coeff mob)]
    (and (= a (c/conjugate d))
         (= c (c/conjugate b))
         (= (determinant mob) (z 1)))))

(defn reduction-distance
  [mob point]
  {:pre [(disc-mobius? mob)]}
  (hyp/distance :disc
                (apply-mobius mob point)
                (z 0)))

(defn reduce-mobius
  ([H mob] (reduce-mobius H mob (z 0)))
  ([H mob point]
   (loop [gamma-it mob
          delta-it mob-id]
     (let [reduced-comp (fn [g]
                          (reduction-distance
                           (comp-mobius g gamma-it) point))
           min-choice (fn [g h]
                        (if (< (reduced-comp g)
                               (reduced-comp h))
                          g
                          h))
           gamma-min (reduce min-choice H)
           red-min (reduced-comp gamma-min)
           red-gamma (reduction-distance gamma-it point)]
       (if (<= red-gamma red-min)
         gamma-it
         (recur (comp-mobius gamma-min
                             gamma-it)
                (comp-mobius gamma-min
                             delta-it)))))))
