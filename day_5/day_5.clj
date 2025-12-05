(require '[clojure.string :as str])

(defn range-string->range [range-string] (map parse-long (-> range-string (str/split #"-"))))

(defn range-string->fresh?
  [range-string]
  (let [[start end] (range-string->range range-string)]
    #(and (>= % start) (<= % end))))

(defn part-1-process-file
  [file-path]
  (let [[fresh-range-segment available-id-segment] (-> file-path slurp str/trim (str/split #"\n\n"))
        available-ids (->> available-id-segment str/split-lines (map parse-long))
        fresh-checks (map range-string->fresh? (str/split-lines fresh-range-segment))
        fresh? (fn [val] (some #(% val) fresh-checks))
        fresh-available-ids (filter fresh? available-ids)]
    (count fresh-available-ids)))

(defn range-union-reducer [ranges new-range]
  (letfn [(overlap? [[s1 e1] [s2 e2]] (or (and (>= e1 s2) (<= s1 s2)) (and (>= e2 s1) (<= s2 s1))))
          (union-ranges [& rs] [(apply min (map first rs)) (apply max (map second rs))])]
   (let [groups (group-by #(overlap? new-range %) ranges)
         overlapping (get groups true)]
    (if (empty? overlapping)
      (conj ranges new-range)
      (let [non-overlapping (get groups false)
            unified-overlapping (apply union-ranges (conj overlapping new-range))]
        (conj non-overlapping unified-overlapping))))))

(defn part-2-process-file
  [file-path]
  (let [ranges (->> file-path slurp str/trim (re-seq #"\d+-\d+") (map range-string->range))
        simplified-ranges (reduce range-union-reducer [] ranges)]
    (transduce (map #(inc (- (second %) (first %)))) + simplified-ranges)))

(assert (= (part-1-process-file "sample.txt") 3))
(prn (part-1-process-file "input.txt"))

(assert (= (part-2-process-file "sample.txt") 14))
(prn (part-2-process-file "input.txt"))
