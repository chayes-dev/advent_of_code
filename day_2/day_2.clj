(require '[clojure.string :as str])

(defn silly-pattern?
  ([pattern]
   (let [pattern-length (count pattern)
         partition-sizes (filter #(= (mod pattern-length %) 0) (range 1 (inc (quot pattern-length 2))))]
     (boolean (some #(silly-pattern? pattern %) partition-sizes))))
  ([pattern partition-size]
   (let [parts (partition partition-size pattern)]
     (= (count (set parts)) 1))))

(defn silly-patterns-in-range
  [[start end] & {:keys [part-1] :or {part-1 false}}]
  (let [patterns (map str (range (parse-long start) (inc (parse-long end))))
        silly? (if part-1 #(silly-pattern? % (/ (count %) 2)) silly-pattern?)]
    (filter silly? patterns)))

(defn process-file
  [file-path & {:keys [part-1] :or {part-1 false}}]
  (let [str-ranges (-> file-path slurp str/trim (str/split #","))
        ranges (map #(str/split % #"-") str-ranges)
        silly-patterns (apply concat (map #(silly-patterns-in-range % :part-1 part-1) ranges))]; (map #(str/split % #"-")))] ;(map #(str/split % #"-")))]
    (reduce + (map parse-long silly-patterns))))

(assert (= (process-file "day_2/sample.txt" :part-1 true) 1227775554))
(prn (process-file "day_2/input.txt" :part-1 true))

(assert (= (process-file "day_2/sample.txt") 4174379265))
(prn (process-file "day_2/input.txt"))
