(require '[clojure.string :as str])

(defn get-amount [turn] (-> turn rest str/join Integer/parseInt))

(defn do-turn [current turn]
  (let [amount (get-amount turn)
        op (if (str/starts-with? turn "R") + -)]
    (mod (op current amount) 100)))

(assert (= (do-turn 50 "L68") 82))
(assert (= (do-turn 52 "R48") 0))

(defn part-1-reducer
  [{:keys [current zeros] :as state} turn & {:keys [verbose] :or {verbose true}}]
  (let [next (do-turn current turn)
        next-state {:current next :zeros (+ zeros (if (= next 0) 1 0))}]
    (when verbose
      (prn state turn next-state))
    next-state))

(defn part-1-run-file
  [file-path]
  (->> file-path slurp str/split-lines (reduce part-1-reducer {:current 50 :zeros 0}) :zeros))

(prn (part-1-run-file "sample.txt"))
;(prn (run-file-part-1 "input.txt"))
;
(defn turn-hits-or-crosses?
  [current turn]
  (let [amount (mod (get-amount turn) 100)]
    (and (not= current 0)
         (if (str/starts-with? turn "L")
          (<= (- current amount) 0)
          (>= (+ current amount) 100)))))

(assert (turn-hits-or-crosses? 50 "L68"))
(assert (not (turn-hits-or-crosses? 82 "L30")))
(assert (turn-hits-or-crosses? 52 "R48"))
(assert (not (turn-hits-or-crosses? 0 "L5")))

(defn part-2-reducer
  [{:keys [current clicks] :as state} turn & {:keys [verbose] :or {verbose true}}]
  (let [hit-or-cross (if (turn-hits-or-crosses? current turn) 1 0)
        full-rotations (quot (get-amount turn) 100)
        next (do-turn current turn)
        next-state {:current next :clicks (+ clicks full-rotations hit-or-cross)}]
    (when verbose
      (prn state turn next-state))
    next-state))


(defn part-2-run-file
  [file-path]
  (->> file-path slurp str/split-lines (reduce part-2-reducer {:current 50 :clicks 0}) :clicks))

(prn (part-2-run-file "sample.txt"))
(prn (part-2-run-file "input.txt"))
