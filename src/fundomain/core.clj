(ns fundomain.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defstruct mobius :coeff)

(def mobius-coeffs
  (accessor mobius :coeff))

(defn mobius?
  [t]
  (let [coeffs (map count (mobius-coeffs t))]
    (and (= (count coeffs) 2)
         (every? #(= % 2)
                 coeffs))))

(defn apply-mobius
  [f z]
  (when (mobius? f)
    (reduce / (map #(+ (* (first %) z)
                       (second %)) (mobius-coeffs f)))))

(defn mobiusf
  [f]
  (partial apply-mobius f))



