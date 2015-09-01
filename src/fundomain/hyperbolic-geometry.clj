(ns fundomain.hyperbolic-geometry
  (:require [fundomain.cnum :refer [complex? z conjugate abs] :as c]))


(defmulti in-model? (fn [model _] model))

(defmulti distance (fn [model point1 point2]
                     {:pre [(in-model? model point1)
                            (in-model? model point2)]} model))


(defmethod in-model? :disc [_ point]
  (and (complex? point)
       (< (c/abs2 point)
          1)))

(defmethod in-model? :upper-half-plane [_ point]
  (and (complex? point)
       (> (:imag point) 0)))

(defmethod distance :disc [_ point1 point2]
  (let [primary (c/abs (c/- (c/z 1)
                            (c/* point1
                                 (c/conjugate point2))))
        secondary (c/abs (c/- point1
                              point2))]
    (Math/log (/ (+ primary secondary)
                 (- primary secondary)))))

(defmethod distance :upper-half-plane [_ point1 point2]
  (let [primary (c/abs (c/- point1
                            (c/conjugate point2)))
        secondary (c/abs (c/- point1
                              point2))]
    (Math/log (/ (+ primary secondary)
                 (- primary secondary)))))

