(require '[clojure.string :as str])
(require '[clojure.math :as math])

(->> (slurp "./inputs/input1.txt")
     (str/split-lines)
     (mapv (fn [t] (vec (drop 1 (re-find #"(R|L)(\d+)" t)))))
     (mapv (fn [t] (update t 1 (fn [x] (* (Integer/parseInt x) (if (= "L" (first t)) 1 -1))))))
     (def turns))

(def turns_simple (vec (for [q turns] (peek q))))

(defn fullrotations [x]
  (int (math/floor (/ (abs x) 100))))


(defn get_next_position [start turn]
  (let [u (if (>= turn 0) (+ start (mod turn 100)) (- start (mod (* turn -1) 100)))]
    (cond-> u
      (< u 0) (+ 100)
      (>= u 100) (- 100)
      )
    )
  )

;; gets the zeros past from x to the next position

(defn zeros-passed [start turn]
  (let [end (+ start turn)]
    (cond
      (= start end) 0

      (> end start)
      (- (long (Math/floor (/ end 100.0)))
         (long (Math/floor (/ start 100.0))))

      :else
      (- (long (Math/ceil (/ start 100.0)))
         (long (Math/ceil (/ end 100.0)))))))

(loop [startpos 50
       instruct turns_simple
       zerospast 0]
  (if (empty? instruct)
    zerospast
    (let [nextturn (first instruct)]
      (recur (get_next_position startpos nextturn) (subvec instruct 1) (+ zerospast (zeros-passed startpos nextturn)))
      )
    )
  )