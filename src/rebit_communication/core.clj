(ns rebit-communication.core
  (:gen-class))

(def Grain 0.0001) ; How finely to divide the continuum of measurements.

(def pi Math/PI)
(def cos #(Math/cos %))
(def pow #(Math/pow %1 %2))

(def non-zero? (complement zero?))

(def rebit-search-space (range 0 (* 2 pi) Grain))

(defn square-inner-product
  [phi psi]
  (pow (cos (/ (- phi psi) 2)) 2)) ; Expression follows from some easy trig.

(defn black?
  "Returns 1 if phi is coloured black on cirle n, 0 otherwise."
  [n phi]
  (if (odd? n) ; Exploit the antisymmetry of circle n and cirle n+1.
    (- 1 (black? (dec n) phi))
    (if (> phi pi) ; Exploit the antisymmetry of antipodal points.
      (- 1 (black? n (- phi pi)))
      (let [k (int (* phi (/ (+ n 2) (* 2 pi))))]
        (if (odd? k) 0 1)))))

(declare prob) ; So we can start using it before it's defined.

(defn total-prob-so-far
  "Computes the probability already assigned to psi by earlier circles."
  [psi n phi]
  (->>
   (range 0 n)
   (map (fn [m] (* (prob psi m) (black? m phi))))
   (apply +)))

(defn max-allowed-prob
  "Computes the maximum allowed probability we can asign cirle n given any possible measurement."
  [psi n]
  (->>
   rebit-search-space
   (filter (fn [phi] (= 1 (black? n phi))))
   (map (fn [phi] (- (square-inner-product phi psi)
                     (total-prob-so-far psi n phi))))
   (apply min)))

(def prob
  (memoize (fn [psi n]
    (if (zero? (black? n psi))
      0.0 ; Consider the measurement containing psi and psi-perp...
      (max-allowed-prob psi n)))))

(defn list-probs
  "Given Alice's psi, prints a list of the corresponding probabilities."
  [psi]
  (loop [n 0
         total 0]
    (let [prob-psi-n (prob psi n)
          new-total (+ total prob-psi-n)]
      (println (str "prob " psi " " n " = " prob-psi-n))
      (when (non-zero? prob-psi-n)
        (println (str "    total = " new-total)))
      (recur (inc n)
             new-total))))
