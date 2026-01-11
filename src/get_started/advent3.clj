(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def rawinput (slurp "./inputs/input3.txt"))

(def formatted_input (str/split-lines rawinput))

(defn top-2v [coll] 
  (let [d1 (apply max (subvec coll 0 (- (count coll) 1))) 
        d2 (apply max (subvec coll (inc (.indexOf coll d1))))]
    [d1 d2])
  )

(defn make-ddnum [[d1 d2]]
  (+ (* d1 10) d2)
  )

(defn get-top-two-digits-and-make-dd-num [coll]
  (make-ddnum (top-2v coll))
  )

(->>
 (mapv #(str/split % #"") formatted_input)
 (mapv (fn [x] (mapv #(parse-long %) x)) )
 (mapv #(get-top-two-digits-and-make-dd-num %) )
 (apply + )
 )

