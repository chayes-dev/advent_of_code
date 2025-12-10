(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-line [line]
  (letfn [(find-nums [s] (->> s (re-seq #"\d+") (map parse-long)))]
    {:indicator-lights (into #{} (keep-indexed #(when (= %2 \#) %1)) (re-find #"[\.#]+" line))
     :buttons (->> line (re-seq #"\([\d,]+\)") (map (comp set find-nums)))
     :joltages (->> line (re-find #"\{[\d,]+\}") find-nums vec)}))

(defn parse-file [file-path]
  (let [lines (-> file-path slurp str/split-lines)]
    (map parse-line lines)))

(defn set-xor
  ([] #{})
  ([s] s)
  ([s1 s2]
   (set/difference (set/union s1 s2) (set/intersection s1 s2)))
  ([s1 s2 & ss]
   (reduce set-xor (concat (list s1 s2) ss))))

(defn combinations
  ([coll] (into #{} (mapcat #(combinations coll %) (range (inc (count coll))))))
  ([coll n]
   (cond
     (zero? n) #{#{}}
     (= (count coll) n) #{(set coll)}
     :else (let [[[v] vs] (split-at 1 coll)
                  use-it (into #{} (for [comb (combinations vs (dec n))] (set (cons v comb))))
                  lose-it (combinations vs n)]
              (set/union use-it lose-it)))))

(defn min-buttons-lights [{:keys [indicator-lights buttons]}]
  (let [success? (fn [button-combination] (= indicator-lights (apply set-xor button-combination)))]
    (->> buttons combinations (sort-by count) (some #(when (success? %) (count %))))))

(defn part-1-process-file [file-path]
  (->> file-path parse-file (map min-buttons-lights) (apply +)))

(assert (= (part-1-process-file "sample.txt") 7))
(time (prn (part-1-process-file "input.txt")))

(defn sum-vecs [& vs] (into [] (map #(apply + %) (apply map vector vs))))
(defn scale-vec [scale v] (into [] (map #(* scale %) v)))
(defn button->vec [length button] (vec (for [i (range length)] (if (button i) 1 0))))
(defn weighted-sum [vs weights] (apply sum-vecs (map scale-vec weights vs)))

(assert (= (scale-vec 3 [1 2 3]) [3 6 9]))
(assert (= (sum-vecs [1 2 3] [4 5 6]) [5 7 9]))
(assert (= (button->vec 5 #{2 3}) [0 0 1 1 0]))
(assert (= (weighted-sum [[0 1 0 1 0] [0 0 1 0 1] [0 0 0 1 0]] [1 2 4]) [0 1 2 5 2]))

(defn push-buttons
  ([buttons] (partial push-buttons buttons))
  ([buttons weights] (let [max-index (apply max (apply set/union buttons))
                           button-vecs (map #((memoize button->vec) max-index %) buttons)
                           scaled-vecs (map scale-vec weights button-vecs)]
                       (apply sum-vecs scaled-vecs))))


(assert (= (push-buttons [#{1 3} #{2 4} #{3}] [1 2 4]) [0 1 2 5]))
