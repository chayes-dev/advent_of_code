(require '[clojure.string :as str])
(require '[clojure.walk :as walk])

(defn part-1-process-file [file-path]
  (let [lines (-> file-path slurp str/trim str/split-lines)
        vals (map (comp #(str/split % #"\s+") str/trim) lines)
        ops (->> vals last (map read-string))
        nums-by-col (->> vals butlast (walk/walk #(map parse-long %) seq) (apply map list))
        totals (map #(eval (cons %1 %2)) ops nums-by-col)]
    (reduce + totals)))

(defn- pad-char-arrays
  [xs]
  (let [longest (apply max (map count xs))
        pad (fn [x] (concat x (repeat (- longest (count x)) \space)))]
    (map pad xs)))

(defn part-2-process-file [file-path]
  (let [lines (-> file-path slurp str/trim str/split-lines)
        ops (->> lines last str/trim (#(str/split % #"\s+")) (map read-string))
        nums-by-col (->> lines
                         butlast
                         (map char-array)
                         pad-char-arrays
                         (apply map list)
                         (map (comp parse-long str/trim str/join))
                         (partition-by nil?)
                         (take-nth 2))
        totals (map #(eval (cons %1 %2)) ops nums-by-col)]
    (reduce + totals)))

(assert (= (part-1-process-file "sample.txt") 4277556))
(prn (part-1-process-file "input.txt"))

(assert (= (part-2-process-file "sample.txt") 3263827))
(prn (part-2-process-file "input.txt"))
