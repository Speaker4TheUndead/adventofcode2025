(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def rawinput (slurp "./inputs/input5.txt"))

(def ranges-and-ids (str/split rawinput #"\n\n"))

(def ranges (str/split-lines(ranges-and-ids 0)))
(def ids (mapv parse-long (str/split-lines (ranges-and-ids 1))))

;; Parse the strings to longs
(def ranges (mapv (fn [x] (mapv #(parse-long %) (str/split x #"-"))) ranges))


(defn in-range? [[low high] num]
  (and (>= high num) (<= low num))
  )


(def test-ranges [[3 5] [10 14] [16 20] [12 18]])
;;(def test-ids [1 5 8 11 17 32])

(comment (count (set (for [ids test-ids
             ranges test-ranges
             :when (in-range? ranges ids)]
         ids))))


(count (set (for [ids ids
      ranges ranges 
      :when (in-range? ranges ids)]
  ids
  )))

;; Part 2
(sort [[1 5] [1 4] [3 4]])
(defn condense-ranges [ranges] 
  (let [[lr1 hr1] (ranges 0)
        [lr2 hr2] (ranges 1)]
    (if (>= hr1 lr2) 
      [lr1 hr2] 
      (ranges 1)
      )
    )
  )

(defn condense-ranges-r [ranges]
  (if (<= 1 (count ranges))
    ranges
    (let [ranges (sort ranges)
          ]
    (if (or (<= (count ranges) 1) ())
      ranges
      
      ) 
    )
    )
  )

(condense-ranges (vec (take 2 (sort [[1 5] [1 4] [3 7]]))))

(count (set (mapcat identity (for [[low high] ranges]
  (range low (inc high))
  ))))
