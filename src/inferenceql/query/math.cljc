(ns inferenceql.query.math)

(defn exp
  "Returns Euler's number e raised to the power of a double value."
  [x]
  (Math/exp x))

(defn median
  "Computes the median of the numerical values in a collection."
  [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (/ (+ bottom-val top-val)
           2)))))
