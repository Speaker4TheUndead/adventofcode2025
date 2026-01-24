(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def rawinput (slurp "./inputs/input4.txt"))

(def formatted_input (str/split-lines rawinput))

(def test-input ["..@@.@@@@."
"@@@.@.@.@@"
"@@@@@.@.@@"
"@.@@@@..@."
"@@.@@@@.@@"
".@@@@@@@.@"
".@.@.@.@@@"
"@.@@@.@@@@"
".@@@@@@@@."
"@.@.@@@.@."])

(defn get-adjacent [matrix x y]
  (for [row [(dec y) y (inc y)]
        col [(dec x) x (inc x)]
        :let [row-max (count matrix)
              idx-max (count (matrix 0))
              ] 
        :when (and (< row row-max) (>= row 0) (< col idx-max) (>= col 0) (not (and (= row y) (= col x))))]
    ;;(list col row)
    (.charAt (matrix row) col)
    )
  )

(defn get-movable-roll-count [matrix]
  (for [y (range (count matrix))
        x (range (count (matrix 0)))
        :when (= (.charAt (matrix y) x) \@)
        ]
    (->>
     (get-adjacent matrix x y)
     (filter #(= % \@))
     (count)
     (> 4))
    )
  )

(count (filter #(= % true) (get-movable-roll-count formatted_input)))