(require '[clojure.string :as str])
(require '[clojure.math :as math])

(defn max-joltage [batteries remaining]
  (if (= remaining 0) 0
   (let [drop-last-n (fn [s n] (->> s reverse (drop n) reverse))
         next-selection (apply max (drop-last-n batteries (dec remaining)))
         remaining-batteries (rest (drop-while #(not= next-selection %) batteries))
         joltage-remaining (max-joltage remaining-batteries (dec remaining))]
     (long (+ (* next-selection (math/pow 10 (dec remaining))) joltage-remaining)))))

(defn process-file [file-path n]
  (letfn [(line-to-seq [line-str] (map (comp parse-long str) (seq line-str)))]
   (->> file-path slurp str/split-lines (map line-to-seq) (map #(max-joltage % n)) (reduce +))))

(assert (= (max-joltage [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1] 2) 98))
(assert (= (process-file "day_3/sample.txt" 2) 357))

(prn (process-file "day_3/input.txt" 2))
(prn (process-file "day_3/input.txt" 12))
