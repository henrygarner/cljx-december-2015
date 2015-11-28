(ns example.async
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as impl]
            [org.httpkit.client :as http]))

;; (defn to-proc< [in]
;;   (let [out (async/chan 1)]
;;     (async/pipe in out)
;;     out))
;; 
;; (defn pipeline< [desc c]
;;   (let [p (partition 2 desc)]
;;     (reduce
;;      (fn [prev-c [n f]]
;;        (-> (for [_ (range 10)]
;;              (-> (async/map< f prev-c)
;;                  to-proc<))
;;            (async/merge))
;;        (async/map< f prev-c))
;;      c
;;      p)))
;; 
;; #_(let [c (async/chan 10)]
;;     (async/>!! c 42)
;;     (async/<!! (pipeline< [4 inc
;;                            1 inc
;;                            2 dec
;;                            3 str]
;;                           c)))
;; 
;; ;; Ep 1
;; 
;; ;; Map is meant to return a channel
;; 
;; (defn pipe-ext [in out]
;;   (async/go (loop []
;;         (when-some [v (async/<! in)]
;;           (async/>! out v)
;;           (recur)))))
;; 
;; (def log-c (async/chan 1024))
;; 
;; (defn map-ext [in f out]
;;   (async/go (loop []
;;         (when-some [val (async/<! in)]
;;           (let [val (try (f val)
;;                          (catch Throwable ex
;;                            (async/>!! log-c ["Failure: " f val])
;;                            :exception))]
;;             (cond
;;                   (or (seq? val)
;;                       (vector? val))
;;                   ;; Backpressure - don't take values from input
;;                   ;; until output has been 
;;                   (do (async/<! (async/onto-chan out val))
;;                       (recur))
;; 
;;                   (extends? impl/ReadPort (class val))
;;                   (do (async/<! (pipe-ext val out))
;;                       (recur))
;; 
;;                   (identical? val :exception) (recur)
;;                   
;;                   :else (do (async/>! out val)
;;                             (recur))))))
;;       nil))
;; 
;; (defn pipeline<' [desc c]
;;   (let [p (partition 2 desc)]
;;     (reduce
;;      (fn [prev-c [n f]]
;;        (let [out-c (async/chan n)
;;              procs (for [_ (range n)]
;;                      (map-ext prev-c f out-c))
;;              close-chan (async/merge procs)]
;;          (async/take! close-chan (fn [_] (async/close! out-c)))
;;          out-c))
;;      c
;;      p)))
;; 
;; (defn wait-100 [v]
;;   (async/go (async/<! (async/timeout 1000))
;;       v))
;; 
;; (defn pause-rnd [x]
;;   (async/go (async/<! (async/timeout (rand-int 1000)))
;;       x))
;; 
;; #_(defn sink [f c]
;;     (async/go (loop []
;;                 (when-some [v (async/<! c)]
;;                   (f v)
;;                   (recur )))))
;; 
;; #_(let [c (async/chan 10)]
;;   (async/>!! c 42)
;;   (async/<!! (async/into []
;;                    (async/take 10 (pipeline<' [4 inc
;;                                                1 inc
;;                                                2 (fn [x]
;;                                                    (to-chan (range x)))
;;                                                10 pause-rnd]
;;                                               c)))))
;; 
;; #_(defmacro <!!? [c]
;;   `(let [v# (async/<!! ~c)]
;;      (if (instance? Throwable v#)
;;        (throw v#)
;;        v#)))
;; 
;; #_(defmacro go-try [& body]
;;   `(async/go (try ~@body
;;             (catch Throwable ex#
;;               ex#))))
;; 
;; 
;; 
;; #_(async/go (loop []
;;       (println "Got log item " (async/<! log-c))
;;       (recur)))
;; 
;; #_(async/>!! log-c "Error")
;; 
;; #_(try (<!!? (go-try (assert false)))
;;      (catch Throwable ex
;;        "got ex"))
;; 
;; #_(let [c (async/chan 1024)]
;;     (async/>!! c 42)
;;     (async/close! c)
;;     (sink println (pipeline<' [4 inc
;;                                2 range
;;                                3 inc
;;                                3 wait-100
;;                                2 str]
;;                               c)))
;; 
;; 
;; 
;; (defrecord Path [in proc out])
;; 
;; (defn batcher [ms]
;;   (let [inner (async/chan 1)
;;         in (async/chan 1)
;;         proc (async/go (loop [t (async/timeout ms)]
;;                    (let [[v c] (async/alts! [t in])]
;;                      (condp identical? c
;;                        t (do (async/>! inner ::split)
;;                              (recur (async/timeout ms)))
;;                        in (if-not (nil? v)
;;                             (do (async/>! inner v)
;;                                 (recur t))
;;                             (async/close! inner))))))
;;         out (->> (async/partition-by (partial identical? ::split)))]))
;; 
;; 
;; 
;; (def prn-chan (async/chan 10 (map println)))
;; #_(async/onto-chan prn-chan (range 100))
;; 
;; 
;; 
;; (def xform
;;   (map inc))
;;

(def target-urls
  ["http://localhost:3000/echo" "http://localhost:3000/echo?id=2"])

(def urls (take 10 (repeatedly #(rand-nth target-urls))))

(defn async-get [url chan]
  (http/get url (fn [resp]
                  (async/put! chan (slurp (:body resp)))
                  (async/close! chan))))

(let [in  (async/chan 1)
      out (async/chan 1)]
  (async/pipeline-async 1 out async-get in)
  (async/onto-chan in urls)
  (println (async/<!! (async/into [] out))))

(defn fork [in pred out-1 out-2]
  (async/go-loop []
    (when-some [v (async/<! in)]
      (if (pred v)
        (async/>! out-1 v)
        (async/>! out-2 v)))
    (recur)))




