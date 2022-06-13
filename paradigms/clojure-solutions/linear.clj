(defn makeVectorFunction [f]
  (fn [& vectors]
    (apply mapv f vectors)))
(defn makeMatrixFunction [f]
  (fn [& matrix]
    (apply mapv (makeVectorFunction f) matrix)))
(def v+ (makeVectorFunction +))
(def v- (makeVectorFunction -))
(def vd (makeVectorFunction /))
(def v* (makeVectorFunction *))
(def m+ (makeMatrixFunction +))
(def m- (makeMatrixFunction -))
(def md (makeMatrixFunction /))
(def m* (makeMatrixFunction *))
(defn scalar [& vectors]
  (apply + (apply mapv * vectors)))
(defn vect [v1, v2] [(- (* (nth v1 1) (nth v2 2)) (* (nth v1 2) (nth v2 1)))
                     (- (* (nth v1 2) (nth v2 0)) (* (nth v1 0) (nth v2 2)))
                     (- (* (nth v1 0) (nth v2 1)) (* (nth v1 1) (nth v2 0)))])
(defn v*s [v, s] (mapv (partial * s) v))
(defn m*s [m, s] (mapv (fn [x] (v*s x, s)) m))
(defn m*v [m, v] (mapv (fn [x] (scalar x, v)) m))
(defn transpose [m] (apply mapv vector m))
(defn m*m [m1, m2] (mapv (fn [x] (mapv (fn [y] (scalar x, y)) (transpose m2))) m1))
(defn recursiveShapelessFunction [f]
  (fn rec [& args]
    (if (vector? (nth args 0)) (apply mapv rec args)
                               (apply f args))))
(def s+ (recursiveShapelessFunction +))
(def s- (recursiveShapelessFunction -))
(def sd (recursiveShapelessFunction /))
(def s* (recursiveShapelessFunction *))