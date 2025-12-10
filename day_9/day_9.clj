(require '[clojure.string :as str])

(defn parse-file [file-path]
  (let [lines (->> file-path slurp str/split-lines (map #(str/split % #",")))]
    (map (fn [l] (into [] (map parse-long l))) lines)))

(defn area [rect]
  (let [height (->> rect (map second) (apply (juxt max min)) (apply -) inc)
        width (->> rect (map first) (apply (juxt max min)) (apply -) inc)]
    (* height width)))

(defn rects [points]
  (into #{} (for [p1 points p2 points :when (not= p1 p2)] #{p1 p2})))

(defn part-1-process-file
  [file-path]
  (let [red-tiles (parse-file file-path)
        rects (rects red-tiles)]
    (apply max (map area rects))))

(assert (= (part-1-process-file "sample.txt") 50))
(time (prn (part-1-process-file "input.txt")))

(defn normal-form [rect]
  (let [xs (map first rect)
        ys (map second rect)]
    [[(apply min xs) (apply min ys)] [(apply max xs) (apply max ys)]]))

(defn in-rect
  ([rect]
   (partial in-rect rect))
  ([rect [x y]]
   (let [between (fn [val bot top] (and (> val bot) (< val top)))
          [[x1 y1] [x2 y2]] (normal-form rect)]
      (and (between x x1 x2) (between y y1 y2)))))

(assert (in-rect #{[0 0] [2 3]} [1 1]))
(assert (not (in-rect #{[0 0] [2 3]} [1 4])))
(assert (not (in-rect #{[0 0] [2 3]} [0 3])))

(defn direction [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (if (> y1 y2) \D \U)
    (if (> x1 x2) \L \R)))

(defn points-on-line [[x1 y1 :as p1] [x2 y2 :as p2]]
  (case (direction p1 p2)
    \U (for [y (range y1 (inc y2))] [x1 y])
    \D (for [y (range y2 (inc y1))] [x1 y])
    \R (for [x (range x1 (inc x2))] [x y1])
    \L (for [x (range x2 (inc x1))] [x y1])))

(comment
 (points-on-line [0 0] [3 0])
 (points-on-line [3 0] [0 0])
 (points-on-line [0 0] [0 3])
 (points-on-line [0 3] [0 0])
 (points-on-line [7 1] [7 3]))

(defn part-2-process-file
  [file-path]
  (let [red-tiles (parse-file file-path)
        outline-points (into #{} (mapcat points-on-line red-tiles (rest (cycle red-tiles))))
        valid? (fn [rect] (not-any? (in-rect rect) outline-points))
        rects-by-size (sort-by area > (rects red-tiles))
        largest-valid (some #(when (valid? %) %) rects-by-size)]
    (area largest-valid)))

(assert (= (part-2-process-file "sample.txt") 24))
(time (prn (part-2-process-file "input.txt")))
