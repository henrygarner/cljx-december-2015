(ns example.util
  (:require [clojure.java.io :as io]))

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
