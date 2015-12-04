(ns example.sweet
  (:require [example.util :refer :all]
            [clojure.core.async :as async]
            [criterium.core :refer :all])
  (:import [org.HdrHistogram DoubleHistogram]))

(load-data "data.edn")

;;({:name "A", :fx 0.8, :a 90, :b 50}
;; {:name "B", :fx 0.2, :a 80, :b 80}
;; {:name "C", :fx 0.1, :a 60, :b 40}
;; {:name "D", :fx 0.5, :a 50, :b 70})

(->> (load-data "data.edn")
     (filter relevant?)
     (map convert-currency)
     (map assign-score))

;;({:name "A", :fx 0.8, :a 112.5, :b 62.5, :score 175.0}
;; {:name "B", :fx 0.2, :a 400.0, :b 400.0, :score 800.0}
;; {:name "D", :fx 0.5, :a 100.0, :b 140.0, :score 240.0})

(filter relevant?)

;; IMAGE OF EFFECTS PEDAL

(def xform
  (comp (filter relevant?)
     (map convert-currency)
     (map assign-score)))

;; PICTURE OF EFFECTS BOARD

(sequence xform (load-data "data.edn"))

;;({:name "A", :fx 0.8, :a 112.5, :b 62.5, :score 175.0}
;; {:name "B", :fx 0.2, :a 400.0, :b 400.0, :score 800.0}
;; {:name "D", :fx 0.5, :a 100.0, :b 140.0, :score 240.0})

;; PICTURE OF EFFECTS BOARD HOOKED TO GUITAR

(->> (load-data "data.edn")
     (sequence (comp xform (take 2))))

;;({:name "A", :fx 0.8, :a 112.5, :b 62.5, :score 175.0}
;; {:name "B", :fx 0.2, :a 400.0, :b 400.0, :score 800.0})

(->> (load-data "data.edn")
     (sequence (comp xform (map :score))))

;; (175.0 800.0 240.0)

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) +))

;; 1215.0

;; PICTURE OF GUITAR HOOKED TO SOMETHING ELSE

(defn mean [xs]
  (let [sum   (reduce + xs)
        count (count xs)]
    (when-not (zero? count)
      (/ sum count))))



(defn mean-reducer [accum x]
  (-> (update-in accum [:sum] + x)
      (update-in [:count] inc)))

#_(reduce mean-reducer (range 10))

(reduce mean-reducer {:sum 0 :count 0} (range 10))

(defn mean
  ;; Init
  ([] {:sum 0 :count 0})

  ;; Step
  ([accum x]
   (-> (update-in accum [:count] inc)
       (update-in [:sum] + x)))

  ;; Complete
  ([{:keys [sum count]}]
   (when-not (zero? count)
     (/ sum count))))

(mean (reduce mean (mean) (range 10)))

(transduce (map identity) mean (range 10))

(defn identity-transducer [rf]
  (fn
    ([]      (rf))         ;; Init
    ([acc]   (rf acc))     ;; Complete
    ([acc x] (rf acc x)))) ;; Step

(defn identity-transducer [rf] rf)

(transduce identity mean (range 10))

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) mean))

;; https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance

(defn variance
  ([] [0 0 0])
  ([[count mean sum-of-squares]]
   (/ sum-of-squares (max 1 (dec count))))
  ([[count mean sum-of-squares] x]
   (let [count' (inc count)
         mean'  (+ mean (/ (- x mean) count'))]
     [count' mean'
      (+ sum-of-squares (* (- x mean') (- x mean)))])))

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) variance))


;; IMAGE OF BELL CURVE

;; Completing

(def standard-deviation
  (completing variance #(Math/sqrt (variance %))))

(->> (load-data "data.edn")
     (transduce (comp xform (map :score)) standard-deviation))

;; => 343.6204301260331

;; Normalization

(->> (load-data "data.edn")
     (sequence (map (juxt :a :b))))

;; ([90 50] [80 80] [60 40] [50 70])

#_(->> (load-data "data.edn")
       (transduce (comp xform (map :score))
                  (juxt mean standard-deviation)))

;; PICTURE OF GUITAR SLUG

(defn juxt-r [& rfns]
  (fn
    ([]      (mapv (fn [f]   (f))     rfns))
    ([acc]   (mapv (fn [f a] (f a))   rfns acc))
    ([acc x] (mapv (fn [f a] (f a x)) rfns acc))))

(def rf
  (juxt-r + conj))

(transduce identity rf (range 10))

(def rf
  (juxt-r + ((take 3) conj)))

(transduce identity rf (range 10))

;; PICTURE OF GUITAR SLUG

(defn juxt-reducer [& rfns]
  (fn
    ([]    (mapv (fn [f] (f)) rfns))
    ([acc] (mapv (fn [f a] (f (unreduced a))) rfns acc))
    ([acc x]
     (let [all-reduced? (volatile! true)
           results (mapv (fn [f a]
                           (if-not (reduced? a)
                             (do (vreset! all-reduced?
                                          false)
                                 (f a x))
                             a))
                         rfns acc)]
       (if @all-reduced? (reduced results) results)))))

(def rf
  (juxt-reducer + ((take 3) conj)))

(transduce identity rf (range 10))

;; [45 [0 1 2]]

(transduce identity rf (range 10))

;; [45 []]

;; Don't alias a stateful transducer

(def rf
  ((map inc) +))

(transduce identity rf (range 10))


(defn facet [rf fns]
  (->> (map (fn [f] ((map f) rf)) fns)
       (apply juxt-reducer)))

(def rf
  (facet + [:a :b]))

(->> (load-data "data.edn")
     (transduce identity rf))

;; => [280 240]

;; map isn't stateful

(defn weighted-mean [nf df]
  (let [rf (facet mean [nf df])]
    (completing rf (fn [x]
                     (let [[n d] (rf x)]
                       (when-not (zero? d)
                           (/ n d)))))))

(->> (load-data "data.edn")
     (transduce identity (weighted-mean :a :b)))

;; => 7/6

(defn fuse [kvs]
  (let [rfns (vals kvs)
        rf   (apply juxt-reducer rfns)]
    (completing rf #(zipmap (keys kvs) (rf %)))))

(def rf
  (fuse {:mean mean
         :sd   standard-deviation}))

(->> (load-data "data.edn")
     (transduce (map :a) rf))

;; => {:mean 70, :sd 18.257418583505537}

(def rf
  (fuse {:mean-score ((map :score) mean)
         :fields (facet
                  (fuse {:mean mean
                         :sd   standard-deviation})
                  [:a :b])}))

(->> (load-data "data.edn")
     (transduce xform rf))

;;

;; {:mean-score 405.0,
;;  :fields [{:mean 204.16666666666666,
;;            :sd 169.71176545346918}
;;           {:mean 200.83333333333334,
;;            :sd 176.78258775494078}]}
;;

(require '[clojure.core.reducers :as r])

(defn mean-reducer
  ([] {:sum 0 :count 0})
  ([{:keys [sum count]}]
   (/ sum count))
  ([accum x]
   (-> (update-in accum [:count] inc)
       (update-in [:sum] + x))))

(defn mean-combiner
  ;; Combiner is used for init value
  ([] {:sum 0 :count 0})
  ([{:keys [sum count]}]
   (/ sum count))
  ([a b]
   (merge-with + a b)))

(->> (load-data "data.edn")
     (sequence (comp xform (map :score)))
     (r/fold mean-combiner mean-reducer))

;; YOU WANT PARALLEL?

(defn iqr-reducer
  ([] (DoubleHistogram. 1e8 3))
  ([hist x] (doto hist (.recordValue x))))

(defn iqr-combiner
  ([] (DoubleHistogram. 1e8 3))
  ([a b] (doto a (.add b)))
  ([hist]
   (vector (.getValueAtPercentile hist 25)
           (.getValueAtPercentile hist 75))))

(->> (load-data "data.edn")
     (eduction xform (map :score))
     (r/fold iqr-combiner iqr-reducer)
     (iqr-combiner))

(defn transform [xform f g xs]
  (comp xform (g (transduce xform f xs))))

(defn filter-between [[min max]]
  (filter #(<= min % max)))

(defn normalise [{:keys [mean sd]}]
  (map #(-> % (- mean) (/ sd))))

(def normal-stats
  (fuse {:mean mean :sd standard-deviation}))

(defn normalized-iqr [xform data]
  (-> (transform xform
                 interquartile-range
                 filter-between
                 data)
      (transform normal-stats
                 normalise
                 data)))

(defn post-complete [rf f]
  (completing rf (fn [acc] (f (rf acc)))))

(defn pre-init [rf f]
  ((map f) rf))

#_

(let [data]
  (-> xform
      (transform (pre-init interquartile-range :score)
                 (fn [[from to]]
                   (filter #(>= from (:score %) to)))
                 data)
      (transform normal-stats normalize data)))


#_

(->  xform
     (transform ((map :score) interquartile-range) filter-score)
     (transform identity statistics normalise))

(->> (load-data "data.edn")
     (eduction xform (map :score))
     (r/fold histogram-combiner histogram-reducer)
     (interquartile-range))


(defn fold [n combinef reducef in]
  (->> (for [_ (range n)]
         (async/reduce reducef (reducef) in))
       (async/merge)
       (async/map< reducef)
       (async/reduce combinef (combinef))
       (async/map< combinef)))

#_(->> (load-data "data.edn")
     (sequence (comp xform (map :score)))
     (async/to-chan)
     (fold 2 histogram-combiner histogram-reducer)
     (async/<!!))

(defn fold [n xform reducef combinef in]
  (let [reduced (async/chan n)
        f       (xform reducef)]
    (->> (for [_ (range n)]
           (async/reduce f (f) in))
         (async/merge)
         (async/pipeline-blocking n reduced (map f)))
    (async/go
      (->> (async/reduce combinef (combinef) reduced)
           (async/<!)
           (combinef)))))

(def data (take 10000 (cycle (load-data "data.edn"))))

#_

(with-progress-reporting
  (quick-bench (Thread/sleep 1000)))

#_

(quick-bench
 (->> (async/to-chan data)
      (fold 100 (comp xform (map :score))
            histogram-reducer
            histogram-combiner)
      (async/<!!)))

#_

(quick-bench
 (->> (eduction xform (map :score) data)
      (r/fold 8
              histogram-combiner
              histogram-reducer)))



;; IMAGE OF BOOK

;; LINK TO BOOK GITHUB

;; https://github.com/clojuredatascience

