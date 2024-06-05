(ns gensql.query.perf.util)

;; time+ adapted from the version at https://clojure-goes-fast.com/kb/benchmarking/time-plus/
(let [time*
      (fn time* [^long duration-in-ms f]
        (let [^com.sun.management.ThreadMXBean bean (java.lang.management.ManagementFactory/getThreadMXBean)
              bytes-before (.getCurrentThreadAllocatedBytes bean)
              duration (* duration-in-ms 1000000)
              start (System/nanoTime)
              first-res (f)
              delta (- (System/nanoTime) start)
              deadline (+ start duration)
              tight-iters (max (quot (quot duration delta) 10) 1)]
          (loop [i 1]
            (let [now (System/nanoTime)]
              (if (< now deadline)
                (do (dotimes [_ tight-iters] (f))
                    (recur (+ i tight-iters)))
                (let [i' (double i)
                      bytes-after (.getCurrentThreadAllocatedBytes bean)
                      total-run-time (- now start)
                      t (/ total-run-time i')]
                  {:first-result first-res
                   :time-per-call (/ t 1e9)
                   :total-time (/ total-run-time 1e9)
                   :bytes-alloc-per-call (/ (- bytes-after bytes-before) i')
                   :num-total-iterations i}))))))]

  (defmacro time+
    "Like `time`, but runs the supplied body for the duration in ms and returns:
    - the mean time in s
    - the mean bytes allocated
    - the total number of iterations run
     - the result of the first call, if any

    Total time in milliseconds must be provided as the first argument."
    [duration-in-ms & body]
    `(~time* ~duration-in-ms (fn [] ~@body))))
