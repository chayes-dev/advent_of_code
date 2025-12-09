(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-file [file-path]
  (let [contents (->> file-path slurp str/split-lines)
        initial-beam-position (str/index-of (first contents) "S")
        get-splitter-positions (fn [line] (set (keep-indexed #(when (= %2 \^) %1) line)))
        splitter-positions (map get-splitter-positions (rest contents))]
    [initial-beam-position splitter-positions]))

(defn part-1-propagate-beams
  [{:keys [beam-positions n-splits]} splitter-positions]
  (let [beam-groups (group-by #(contains? splitter-positions %) beam-positions)
        split-beams (get beam-groups true)
        non-split-beams (set (get beam-groups false))
        diffracted-beams (set (mapcat #((juxt inc dec) %) split-beams))
        new-beam-positions (set/union diffracted-beams non-split-beams)]
    {:n-splits (+ n-splits (count split-beams)) :beam-positions new-beam-positions}))

(defn part-1-process-file [file-path]
  (let [[initial-beam-position splitter-positions] (parse-file file-path)
        final-state (reduce part-1-propagate-beams {:beam-positions #{initial-beam-position} :n-splits 0} splitter-positions)]
    (:n-splits final-state)))

(assert (= (part-1-process-file "sample.txt") 21))
(prn (part-1-process-file "input.txt"))

(defn combine-maps
  [op & ms]
  (let [all-keys (mapcat keys ms)
        val-for-key (fn [k] (apply op (map #(get % k (op)) ms)))]
   (apply hash-map (interleave all-keys (map val-for-key all-keys)))))

(assert (= (combine-maps + {:a 1 :b 2} {:b 3 :d 4}) {:a 1 :b 5 :d 4}))

(defn part-2-propagate-beams
  [beam-intensities splitter-positions]
  (let [beam-groups (group-by #(contains? splitter-positions %) (keys beam-intensities))
        split-beams-in (select-keys beam-intensities (get beam-groups true))
        non-split-beams (select-keys beam-intensities (get beam-groups false))
        split-beams-out (mapcat (fn [[k v]] (list {(inc k) v} {(dec k) v})) split-beams-in)]
    (apply combine-maps + (cons non-split-beams split-beams-out))))

(defn part-2-process-file [file-path]
  (let [[initial-beam-position splitter-positions] (parse-file file-path)
        final-state (reduce part-2-propagate-beams {initial-beam-position 1} splitter-positions)]
    (apply + (vals final-state))))

(assert (= (part-2-process-file "sample.txt") 40))
(prn (part-2-process-file "input.txt"))
