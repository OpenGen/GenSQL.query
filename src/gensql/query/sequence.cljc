(ns gensql.query.sequence
  "Functions operating on and returning sequences.")

(defn intersperse
  "Interleaves each pair of items in the first collection with all the items in
  the second."
  [c1 c2]
  (lazy-seq
   (when-let [s1 (seq c1)]
     (when s1
       (cons (first s1)
             (when-let [s1 (seq (rest s1))]
               (lazy-cat c2 (intersperse s1 c2))))))))
