(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :as set])

(defn parse-file
  [file-path]
  (let [lines (->> file-path slurp str/split-lines (map #(str/split % #",")))]
    (map #(into [] (map parse-long %)) lines)))

(defn pairs [xs] (set (for [f xs s xs :when (not= f s)] #{f s})))

(assert (= (pairs [1 2 3]) #{#{1 2} #{1 3} #{2 3}}))

(defn dist [v1 v2] (math/sqrt (apply + (map #(math/pow (- %1 %2) 2) v1 v2))))

(assert (= (dist [0 0] [3 4]) 5.0))
(assert (= (dist [1 2] [4 6]) 5.0))

(defn add-link [{:keys [clusters]} points]
  (let [containing-cluster (fn [p] (some #(when (contains? % p) %) clusters))
        pair-clusters (set (map containing-cluster points))
        retained-clusters (set/difference clusters pair-clusters)
        new-cluster (apply set/union pair-clusters)]
    {:clusters (set (cons new-cluster retained-clusters))}))

(comment (add-link {:clusters #{#{1 2} #{3 4} #{5}}} [4 5]))

(defn agglomerative-cluster [points & {:keys [n-steps reducer] :or {reducer add-link}}]
  (let [point-pairs (pairs points)
        sorted-pairs (into [] (sort-by #(apply dist %) point-pairs))
        singleton-clusters (set (map hash-set points))
        pairs-to-process (if n-steps (take n-steps sorted-pairs) sorted-pairs)]
    (reduce reducer {:clusters singleton-clusters} pairs-to-process)))

(defn part-1-process-file
  [file-path n-steps]
  (let [points (parse-file file-path)
        clusters (:clusters (agglomerative-cluster points :n-steps n-steps))
        cluster-sizes (sort > (map count clusters))
        largest-cluster-sizes (take 3 cluster-sizes)]
    (apply * largest-cluster-sizes)))

(assert (= (-> "sample.txt" parse-file (agglomerative-cluster :n-steps 10 :reducer add-link) :clusters count) 11))
(assert (= (part-1-process-file "sample.txt" 10) 40))
(prn (part-1-process-file "input.txt" 1000))

(defn part-2-reducer [{:keys [clusters] :as state} points]
  (if (> (count clusters) 1)
    (assoc (add-link state points) :latest-points points)
    (reduced state)))

(comment (part-2-reducer {:clusters #{#{[1 0] [2 1]} #{[3 5] [4 2]} #{[5 0]}}} #{[1 0] [3 5]}))

(defn part-2-process-file
  [file-path]
  (let [points (parse-file file-path)
        final-state (agglomerative-cluster points :reducer part-2-reducer)]
    (apply * (map first (:latest-points final-state)))))


(assert (= (part-2-process-file "sample.txt") 25272))
(prn (part-2-process-file "input.txt"))
