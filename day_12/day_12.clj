(require '[clojure.string :as str])

(defn parse-tile-definition
  [tile-definition]
  (letfn [(parse-line [line] (count (filter #(= % \#) line)))]
   (->> tile-definition str/split-lines rest (map parse-line) (apply +))))

(defn parse-region-definition
  [region-definition]
  (let [[dimensions tile-requirements] (-> region-definition (str/split #": "))
        dimensions (map parse-long (str/split dimensions #"x"))
        tile-counts (map parse-long (str/split tile-requirements #" "))]
    {:dimensions dimensions :tile-counts tile-counts}))

(defn parse-file [file-path]
  (let [contents (-> file-path slurp (str/split #"\n\n"))
        region-definitions (str/split-lines (last contents))
        tile-definitions (butlast contents)]
    {:regions (map parse-region-definition region-definitions)
     :tiles (map parse-tile-definition tile-definitions)}))

(defn solvable? [tiles {:keys [dimensions tile-counts]}]
  (let [total-spaces (apply + (map * tiles tile-counts))
        total-area (apply * dimensions)]
    (>= total-area total-spaces)))

(defn process-file [file-path]
  (let [{:keys [regions tiles]} (parse-file file-path)
        solvable-regions (filter #(solvable? tiles %) regions)]
    (count solvable-regions)))

(time (prn (process-file "input.txt")))

(comment
  (def contents (slurp "sample.txt"))
  (str/split contents #"\n\n")
  (take 10 (:regions (parse-file "input.txt")))
  ,)
