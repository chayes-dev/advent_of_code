(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-roll-coords [file-path]
  (letfn [(roll-positions [row] (keep-indexed #(if (= (str %2) "@") %1 nil) row))
          (row-tuples [row-id row-positions] (map #(vector % row-id) row-positions))]
    (let [lines (-> file-path slurp str/split-lines)
          positions-by-row (map (comp roll-positions seq) lines)
          roll-coords (apply concat (map-indexed row-tuples positions-by-row))]
      (set roll-coords))))

(defn accessible? [roll-coords [x y]]
  (let [adjacencies [[(dec x) (dec y)] [(dec x) y] [x (dec y)] [x (inc y)] [(inc x) y] [(inc x) (inc y)] [(dec x) (inc y)] [(inc x) (dec y)]]
        adjacent-rolls (filter #(contains? roll-coords %) adjacencies)]
    (< (count adjacent-rolls) 4)))

(defn get-accessible [roll-coords]
  (set (filter #(accessible? roll-coords %) roll-coords)))

(defn remove-all [roll-coords]
  (let [accessible (get-accessible roll-coords)]
    (if (empty? accessible)
      roll-coords
      (remove-all (set/difference roll-coords accessible)))))

(defn count-removable [roll-coords]
  (let [all-removed (remove-all roll-coords)]
    (- (count roll-coords) (count all-removed))))


(assert (= (-> "day_4/sample.txt" get-roll-coords get-accessible count) 13))
(prn (count (get-accessible (get-roll-coords "day_4/input.txt"))))
(prn (count-removable (get-roll-coords "day_4/input.txt")))
