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

;;; Part 2

(defn make-char-vector [s-matrix]
  (->>
   (mapv #(str/split % #"") s-matrix)
   (mapv (fn [x] (mapv #(.charAt % 0) x)) )
   ) 
  )

;;(def cv-test-input (make-char-vector test-input)) 
(def cv-formatted-input (make-char-vector formatted_input))

(defn get-adjacent-2 [matrix x y]
  (for [row [(dec y) y (inc y)]
        col [(dec x) x (inc x)]
        :let [row-max (count matrix)
              idx-max (count (matrix 0))]
        :when (and (< row row-max) (>= row 0) (< col idx-max) (>= col 0) (not (and (= row y) (= col x))))]
    ;;(list col row)
    ((matrix row) col)
    ))

;;(get-adjacent-2 cv-test-input 0 0) 

(defn get-removable-rolls [matrix]
  (filter #(not (= % nil)) (for [y (range (count matrix))
                    x (range (count (matrix 0)))
                    :when (= ((matrix y) x) \@)]
                (when (> 4 (count (filter #(= % \@) (get-adjacent-2 matrix x y))))
                  (list x y))
                ))
  )

;;(get-removable-rolls cv-test-input)

(defn remove-rolls [matrix]
  (let [original-m matrix
        new-m
        (loop [matrix matrix
               removable-rolls (get-removable-rolls matrix)]
          (if (empty? removable-rolls)
            matrix
            (let [[rx ry] (first removable-rolls)]
              (recur (assoc matrix ry (assoc (matrix ry) rx \X)) (rest removable-rolls)))))]
    (if (= original-m new-m)
      new-m
      (remove-rolls new-m) 
      )
    ) 
  )

(defn count-removed-rolls [matrix]
  (->>
   (mapv (fn [x] (filter #(= % \X) x)) matrix)
   (mapv #(count %) ) 
   (apply + )
   ) 
  )
;;(count-removed-rolls (remove-rolls cv-test-input))
(count-removed-rolls (remove-rolls cv-formatted-input))