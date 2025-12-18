(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[node-id & connections] (str/split line #"[^\w]+")]
    [node-id (set connections)]))

(defn parse-file [file-path]
  (->> file-path slurp str/split-lines (mapcat parse-line) (apply hash-map)))

(defn update-paths
  [paths [src dest :as edge]]
  (let [add-path (fn [ps new-path] (update ps ((juxt first last) new-path) #(conj % new-path)))
        incoming-paths (->> paths (keep (fn [[[_ p-dest] ps]] (when (= p-dest src) ps))) (apply concat))
        outgoing-paths (->> paths (keep (fn [[[p-src _] ps]] (when (= p-src dest) ps))) (apply concat))
        connected-paths (for [ip incoming-paths op outgoing-paths] (vec (concat ip op)))
        incoming-paths-augmented (map #(conj % dest) incoming-paths)
        outgoing-paths-augmented (map #(vec (cons src %)) outgoing-paths)
        new-paths (concat incoming-paths-augmented outgoing-paths-augmented connected-paths [edge])]
    (reduce add-path paths new-paths)))

(defn find-paths [connections]
  (let [node-ids (keys connections)
        paths (into {} (for [from node-ids to node-ids :when (not= from to)] [[from to] #{}]))
        edges (mapcat (fn [[in outs]] (map #(vector in %) outs)) connections)]
    (loop [p paths e edges]
      (if (empty? e) p
          (recur (update-paths p (first e)) (rest e))))))

(defn update-counts [counts [src dest :as edge]]
  (let [incoming-paths (filter (fn [[[_ d] _]] (= d src)) counts)
        counts-with-incoming (into {} (map (fn [[[s _] c]] [[s dest] c]) incoming-paths))
        outgoing-paths (filter (fn [[[s _] _]] (= s dest)) counts)
        counts-with-outgoing (into {} (map (fn [[[_ d] c]] [[src d] c]) outgoing-paths))
        counts-incoming-outgoing (apply hash-map (apply concat (for [[[s-i _] c-i] incoming-paths [[_ d-o] c-o] outgoing-paths] [[s-i d-o] (* c-i c-o)])))]
    (merge-with + counts {edge 1} counts-with-incoming counts-with-outgoing counts-incoming-outgoing)))

(defn count-paths [connections]
  (loop [counts {} edges (mapcat (fn [[in outs]] (map #(vector in %) outs)) connections)]
    (prn (count edges))
    (if (empty? edges) counts
        (recur (update-counts counts (first edges)) (rest edges)))))

(defn part-1-process-file [file-path]
  (-> file-path parse-file count-paths (get ["you" "out"])))

(assert (= (part-1-process-file "sample.txt") 5))
(time (prn (part-1-process-file "input.txt")))

(defn detect-cycles [path visited connections]
  (if (-> path last visited) [nil visited]
      (let [current (last path)
            cycle-with (fn [next] (seq (drop-while #(not= % next) path)))]
        (loop [[next & rst] (connections current) v visited]
          (if (nil? next) [nil (conj v current)]
              (if-let [cycle-with-next (cycle-with next)] [cycle-with-next v]
                      (let [[detected-cycle updated-visited] (detect-cycles (conj path next) v connections)]
                        (if (seq detected-cycle) [detected-cycle updated-visited]
                            (recur rst updated-visited)))))))))

(defn part-2-process-file [file-path]
  (let [connections (parse-file file-path)
        path-counts (count-paths connections)
        first-fft-dac (if (path-counts ["dac" "fft"]) "dac" "fft")
        second-fft-dac ({"fft" "dac" "dac" "fft"} first-fft-dac)]
    (apply * (map path-counts [["svr" first-fft-dac] [first-fft-dac second-fft-dac] [second-fft-dac "out"]]))))

(assert (= (part-2-process-file "sample_2.txt") 2))
(time (prn (part-2-process-file "input.txt")))

(comment
  (def connections (parse-file "input.txt"))
  (time (prn (detect-cycles ["svr"] #{} connections)))
  (def paths (find-paths connections))
  (paths ["svr" "out"])
  (into {} (take 10 paths))
  (part-2-process-file "sample_2.txt"))
