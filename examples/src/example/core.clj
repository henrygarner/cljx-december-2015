(ns example.core
  (:refer-clojure :exclude [juxt])
  (:require [clojure.core.async :as async]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [incanter.stats :refer [cdf-normal]]
            [tesser.core :as t]
            [tesser.math :as m])
  (:import [org.HdrHistogram EncodableHistogram
                             DoubleHistogram
            DoubleHistogramIterationValue]))

(defn load-data [file]
  (->> (io/resource file)
       (io/reader)
       (line-seq)
       (map read-string)))

(defn relevant? [x]
  (> (:b x) 40))

(defn convert-currency [{:keys [fx] :as x}]
  (-> x
      (update-in [:a] / fx)
      (update-in [:b] / fx)))

(defn assign-score [x]
  (let [score (->> (select-keys x [:a :b])
                   (vals)
                   (reduce +))]
    (assoc x :score score)))


(->> (load-data "data.edn")
     (filter relevant?)
     (map convert-currency)
     (map assign-score))

(defn process [data]
  (->> (filter relevant? data)
       (map convert-currency)
       (map assign-score)))

(process (load-data "data.edn"))


(def v [1 2 3 4])
;; #'user/v

(type v)
;; clojure.lang.PersistentVector

(type (map inc v))
;; clojure.lang.LazySeq

(type (mapv inc v))
;; clojure.lang.PersistentVector


(def xform
  (comp (filter relevant?)
     (map convert-currency)
     (map assign-score)))

(sequence xform (load-data "data.edn"))


(->> (load-data "data.edn")
     (sequence (comp xform (take 2))))

(->> (load-data "data.edn")
     (sequence (comp xform (map :score))))

(->> (load-data "data.edn")
     (sequence (comp xform (map :score)))
     (reduce +))

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) +))

(+)
;; 0

(+ 42)
;; 42

(+ 21 21)
;; 42

(conj)
;; []

(conj [42])
;; [42]

(conj [21] 21)
;; [21 21]


(defn hist-iqr
  ;; Zero arity init
  ([] (DoubleHistogram. 1e8 3))
  
  ;; Two arity step
  ([hist x]
   (doto hist
     (.recordValue x)))

  ;; Single arity complete
  ([hist]
   (vector (.getValueAtPercentile hist 25)
           (.getValueAtPercentile hist 75))))

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) hist-iqr))


(defn iqr-sequence [xform data]
  (let [[from to] (->> data
                       (transduce (comp xform (map :score)) hist-iqr))]
    (->> data
         (sequence (comp xform (filter #(<= from (:score %) to)))))))


(defn mean-step
  ([] {:sum 0 :count 0})
  ([accum x]
   (-> (update-in accum [:count] inc)
       (update-in [:sum] + x)))
  ([{:keys [sum count]}]
   (/ sum count)))

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) mean-step))


(defn iqr-mean [xform data]
  (let [[from to] (->> data
                       (transduce (comp xform (map :score)) hist-iqr))]
    (->> data
         (transduce (comp xform
                     (filter #(<= from (:score %) to))
                     (map :score))
                    mean-step))))

(iqr-mean xform (load-data "data.edn"))

(defn mean-combiner
  ;; Combiner is used for init value
  ([] {:sum 0 :count 0})
  ([a b]
   (merge-with + a b)))

(->> (load-data "data.edn")
     (into [] (comp xform (map :score)))
     (r/fold mean-combiner mean-step))


(def scorer
  (comp xform (map :score)))

(->> (load-data "data.edn")
     (into [] scorer)
     (r/fold mean-combiner mean-step))

(->> (load-data "data.edn")
     (r/fold mean-combiner (scorer mean-step)))


(defn filter' [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (pred input)
         (rf result input)
         result)))))

(transduce (filter' even?) conj [1 2 3 4 5 6])

;; [2 4 6]

(def mean-fold
  (->> (t/filter relevant?)
       (t/map convert-currency)
       (t/map assign-score)
       (t/map :score)
       (m/mean)))

(-> (t/chunk 1024 (load-data "data.edn"))
    (t/tesser mean-fold))

(def iqr-transform
  {:reducer-identity (fn [] (DoubleHistogram. 1e8 3))
   :reducer (fn [hist x]
              (doto hist
                (.recordValue x)))
   :post-reducer identity
   :combiner-identity (fn [] (DoubleHistogram. 1e8 3))
   :combiner (fn [a b]
               (doto a (.add b)))
   :post-combiner (fn [hist]
                    (vector (.getValueAtPercentile hist 25)
                            (.getValueAtPercentile hist 75)))})

(def iqr-fold
  (->> (t/filter relevant?)
       (t/map convert-currency)
       (t/map assign-score)
       (t/map :score)
       (t/fold iqr-transform)))

(-> (t/chunk 1024 (load-data "data.edn"))
    (t/tesser iqr-fold))

;; [175.0 240.0]


(def multi-iqr-fold
  (->> (t/filter relevant?)
       (t/map convert-currency)
       ;; Facet runs fold on all keys in a map
       (t/map #(select-keys % [:measure-a :measure-b]))
       (t/facet)
       (t/fold iqr-transform)))

(-> (t/chunk 1024 (load-data "data.edn"))
    (t/tesser multi-iqr-fold))

;; {:measure-a [100.0 112.5] :measure-b [62.5 140.09375]}

(def fused-fold
  (->> (t/filter relevant?)
       (t/map convert-currency)
       (t/map assign-score)
       ;; Fuse runs different named folds
       (t/fuse {:count (t/count)
                :iqr   (->> (t/map :score)
                            (t/fold iqr-transform))})))

(-> (t/chunk 1024 (load-data "data.edn"))
    (t/tesser fused-fold))



(defn calculate-coefficients [{:keys [covariance variance-x
                                      mean-x mean-y]}]
  (let [slope (/ covariance variance-x)]
    {:intercept (- mean-y (* mean-x slope))
     :slope slope}))

(defn linear-regression [fx fy fold]
  (->> fold
       (t/fuse {:covariance (m/covariance fx fy)
                :variance-x (m/variance (t/map fx))
                :mean-x (m/mean (t/map fx))
                :mean-y (m/mean (t/map fx))})
       (t/post-combine calculate-coefficients)))

(def linear-regression-fold
  (->> (t/filter relevant?)
       (t/map convert-currency)
       (linear-regression :a :b)))

(-> (t/chunk 1024 (load-data "data.edn"))
    (t/tesser linear-regression-fold))


(defn rand-file [path]
  (io/file path (str (long (rand 0x100000000)))))

(defn linear-regression-dfold [fx fy]
  (->> (t/map read-string)
       (t/filter relevant?)
       (t/map convert-currency)
       (linear-regression fx fy)))


(defn hist-r
  ;; Zero arity init
  ([] (DoubleHistogram. 1e8 3))
  
  ;; Two arity step
  ([hist x]
   (doto hist
     (.recordValue x)))

  ;; Single arity complete
  ([hist] hist))

(defn hist-c
  ;; Zero arity init
  ([] (DoubleHistogram. 1e8 3))

  ;; Two arity step
  ([a b]
   (doto a (.add b)))

  ;; Single arity complete
  ([hist] hist))


(defn pipeline-n [n in]
  (let [cs (for [_ (range 10)]
             (async/chan 1))]
    (async/go
      (while true
        (let [x (async/<! in)
              outs (map #(vector % x) cs)]
          (async/alts! outs))))
    cs))



;; Pipeline is like map
;; We want to do a reduce


(defn pipeline' [n to xf from close?]
  (let [ex-handler (fn [ex]
                     (-> (Thread/currentThread)
                         .getUncaughtExceptionHandler
                         (.uncaughtException (Thread/currentThread) ex))
                     nil)
        jobs    (async/chan n)
        results (async/chan n)
        process (fn [[v p :as job]]
                  (if (nil? job)
                    (do (async/close! results) nil)
                    (let [res (async/chan 1 xf ex-handler)]
                      (async/>!! res v)
                      (async/close! res)
                      (async/put! p res)
                      true)))]
    (dotimes [_ n]
      (async/go-loop []
        (let [job (async/<! jobs)]
          (when (process job)
            (recur)))))
    (async/go-loop []
      (let [v (async/<! from)]
        (if (nil? v)
          (async/close! jobs)
          (let [p (async/chan 1)]
            (async/>! jobs [v p])
            (async/>! results p)
            (recur)))))
    (async/go-loop []
      (let [p (async/<! results)]
        (if (nil? p)
          (when close? (async/close! to))
          (let [res (async/<! p)]
            (loop []
              (let [v (async/<! res)]
                (when (and (not (nil? v)) (async/>! to v))
                  (recur))))
            (recur)))))))


#_(defn parallel-mean []
  (let [xs  (range 10000)
        in  (async/chan 1)
        out (async/chan 10)]
    (async/onto-chan in xs)
    ;; (pipeline-n 10 in)
    (pipeline-r 10 out (map str) in true)
    (async/<!! (async/into [] out))))


;; Pipeline

(defn pipeline-r [n f g xs]
  (let [in (async/chan n)]
    (async/onto-chan in xs)
    (->> (for [_ (range n)]
           (async/reduce f (f) in))
         (async/merge)
         (async/reduce g (g))
         (async/<!!)
         (f))))

(defn api-call [url]
  (let [chan (async/chan 1)]
    (println url)
    (http/get url (fn [resp]
                    (async/put! chan resp)
                    (async/close! chan)))
    chan))

(defn promise-c [in-c]
  (let [out-c (async/chan 1)]
    (async/go (let [value (async/<! in-c)]
                (while true
                  (async/>! out-c value))))
    out-c))

(defn cache-fn [f]
  (let [cache (atom {})]
    (fn [& args]
      (if-let [result (get @cache args)]
        result
        (let [ret-c (promise-c (apply f args))]
          (swap! cache assoc args ret-c)
          ret-c)))))

(def cached (cache-fn api-call))

(defn cache-fn1 [hit miss]
  (let [cache (atom {})]
    (fn [& args]
      (async/go
        (if-let [result (get @cache args)]
          (async/>! hit  args)
          (async/>! miss args))))))

(defn cached [in hit miss]
  (let [cache (atom {})]
    (async/go-loop []
      (if-some [v (async/<! in)]
        (do (println "inside cashed for " v)
            (if (get @cache v)
              (do (println "cache hit!") (async/>! hit v))
              (do (println "cache miss")
                  (swap! @cache assoc v true)
                  (async/>! miss v)))
            (recur))
        (do (println "Closing down...")
            (async/close! in)
            (async/close! hit)
            (async/close! miss))))))

(def target-urls ["http://google.com" "http://yahoo.com" "http://bing.com"])

(def urls (take 10 (repeatedly #(rand-nth target-urls))))

(defn async-get [url chan]
  (http/get url (fn [resp]
                  (async/put! chan resp)
                  (async/close! chan))))

(defn blahblah []
  (let [urls-chan (async/chan 10 distinct)]
    (println (async/<!! (async/into [] urls-chan)))
    (async/onto-chan urls-chan (range 100))
    ;; (cached urls-chan hit-chan miss-chan)
    ;;(async/pipeline-async 5 out-chan async-get miss-chan true)
    ))


(def xform
  (map inc))

(def cf +)

(def spec
  {:something [xform cf]
   :something-else [xform cf]})

(defn mean-step
  ([] {:sum 0 :count 0})
  ([accum x]
   (-> (update-in accum [:count] inc)
       (update-in [:sum] + x)))
  ([{:keys [sum count]}]
   (/ sum count)))

(defn fuse [named-steps]
  (fn ([]
      (reduce (fn [m [k v]]
                (assoc m k (v))) {} named-steps))
    ([accum x]
     (reduce (fn [m [k v]]
               (update-in m [k] v x)) accum named-steps))
    ([accum]
     (reduce (fn [m [k v]]
               (update-in m [k] v)) accum named-steps))))

(defn combine [named-steps]
  (let [f (fuse named-steps)]
    (fn
      ([] (f))
      ([accum x]
       (reduce (fn [m [k v]]
                 (update-in m [k] v (k x))) accum named-steps))
      ([accum] (f accum)))))

(defn facet [f]
  (fn
    ([] {})
    ([accum x] (merge-with f accum x))
    ([accum] accum)))


(transduce (comp (map inc))
           (fuse {:one + :two +})
           (range 10))


(defn pipeline-r [n f g xs]
  (let [in (async/chan n)]
    (async/onto-chan in xs)
    (->> (for [_ (range n)]
           (async/reduce f (f) in))
         (async/merge)
         (async/reduce g (g))
         (async/<!!)
         (g))))

(defn mean-step
  ([] {:sum 0 :count 0})
  ([accum x]
   (-> (update-in accum [:count] inc)
       (update-in [:sum] + x)))
  ([{:keys [sum count]}]
   (/ sum count)))

(defn mean-combiner
  ;; Combiner is used for init value
  ([] {:sum 0 :count 0})
  ([a b]
   (merge-with + a b))
  ([{:keys [sum count]}]
   (/ sum count)))

#_(pipeline-r 20
              (fuse {:one ((getter :x) +)})
              (combine {:one +})
            (map #(hash-map :x %) (range 10)))

(m/standard-deviation)

#_{:reducer-identity (constantly [0 0 0])
   :reducer (fn count-mean-sq [[count mean sum-of-squares] x]
              (let [count' (inc count)
                    mean'  (+ mean (/ (- x mean) count'))]
                [count'
                 mean'
                 (+ sum-of-squares (* (- x mean') (- x mean)))]))
   :post-reducer identity
   :combiner-identity (constantly [0 0 0])
   :combiner (fn partcmsq [[c m sq] [c2 m2 sq2]]
               (let [count (+ c c2)]
                 (if (zero? count)
                   [c m sq]
                   [count
                    (/ (+ (* c m) (* c2 m2)) count)
                    (+ sq sq2 (/ (* (- m2 m) (- m2 m) c c2) count))])))
   :post-combiner (fn vardiv [x] (double (/ (last x) (core/max 1 (dec (first x))))))}

(defn sd-step
  ([] [0 0 0])
  ([[count mean sum-of-squares] x]
   (let [count' (inc count)
         mean'  (+ mean (/ (- x mean) count'))]
     [count'
      mean'
      (+ sum-of-squares (* (- x mean') (- x mean)))]))
  ([[count mean sum-of-squares]]
   (/ sum-of-squares (max 1 (dec count)))))

(defn project [fns step]
  (fuse (into {} (map #(vector % ((map %) step)) fns))))

;; comp (take 2) doesn't work - use reduced?
(let [step (fuse {:mean mean-step
                  :iqr  hist-iqr
                  :sd   sd-step})]
  (transduce (map identity)
             (project [:x :y] step)
             (map #(hash-map :x %1 :y %2) (range 10) (range 2 12))))

(defn normal-indicator [f mean sd]
  (fn [x]
    (-> (f x)
        (cdf-normal :mean mean :sd sd))))

(defn juxt [& rfns]
  (let [rfns (vec rfns)]
    (fn
      ([] (mapv #(vector % (volatile! (%))) rfns))
      ([acc] (mapv (fn [[rf vacc]] (rf (unreduced @vacc))) acc))
      ([acc x]
       (let [some-unreduced (reduce (fn [some-unreduced [rf vacc]] 
                                      (when-not (reduced? @vacc)
                                        (vswap! vacc rf x) true))
                                    false acc)]
         (if some-unreduced acc (reduced acc)))))))

(def normal-step
  (juxt mean-step sd-step))

completing

(defn wrapping [rf f]
  (fn
    ([] (rf))
    ([x] (rf x))
    ([x y] (rf x (f y)))))

(defn wrapping-completion [rf f]
  (fn
    ([] (rf))
    ([x] (f (rf x)))
    ([x y] (rf x y))))

(defn facet [rf fns]
  (->> (mapv (fn [f] (wrapping rf f)) fns)
       (apply juxt)))

(transduce (comp (map identity) (take 10)) (fuse {:mean mean-step :hist hist-iqr}) (range 1000))

(defn transmap [xform f g xs]
  (let [reduced (transduce xform f xs)
        g'      (fn [x] (g x reduced))]
    (comp xform (map g'))))

#_(-> (map identity)
      (transmap summary-stats normalise data)
      (transmap calculate-factor factorize data))

;; Can calculate on summary of data on sample of data


(def  ks [:a :b])
(def  f  (facet normal-step ks))
(defn g [x aggregates]
  (let [g (fn [k [mean sd]]
            (cdf-normal (k x) :mean mean :sd sd))]
    (map g ks aggregates)))

#_(xtransform xform f g)

(defn pipeline
  "Uses xs to adjust xform by appending normalization step"
  [ks]
  (fn [xform xs]
    (let [aggregates (transduce xform (facet normal-step ks) xs)
          f (fn [x aggregates]
              (let [g (fn [k [mean sd]]
                        (cdf-normal (k x) :mean mean :sd sd))]
                (map g ks aggregates)))]
      (comp xform (map f)))))

#_(defn normalize [[mean sd] x]
    (cdf-normal x :mean mean :sd sd))

(def normalize
  (pipeline [:a :b]))

#_(let [data (load-data "data.edn")]
  (-> (comp (map identity)
         (filter relevant?)
         (map convert-currency))
      (normalize data)
      (sequence data)))

;; Weighted average

(defn weighted-avg [n d]
  (wrapping-completion
   (facet mean-step [n d])
   (fn [[n d]] (if (zero? d) 0 (/ n d)))))

(let [f (weighted-avg :a :b)]
 (reduce f (f) [{:a 1 :b 2} {:a 4 :b 8}]))

;; Can't just pass a transduce to r/fold. Doesn't complete.

;; Step function must honour reduced.
;; Naive implementation of juxt wont.

