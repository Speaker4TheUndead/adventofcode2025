(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def rawinput (slurp "./inputs/input2.txt"))

(def formatted_input (str/split rawinput #","))

;; Functions for Part 2
(defn generate-digit-groups [digits] 
  (comment (def my-digits (loop [digits (subvec digits 0 (Math/floor (/ (count digits) 2)))
                                 groups []]
                            (if (empty? digits)
                              (vec (rseq groups))
                              (recur (subvec digits 0 (- (count digits) 1)) (conj groups (reduce str digits)))
                              )
                            ))
           (filterv (fn [x] (= (mod (count my-digits) (count x)) 0)) my-digits))
  
  (->> 
   (mapv inc (range (Math/floor (/ (count digits) 2))))
   (filterv (fn [x] (= (mod (count digits) x) 0)) )
   (def valid-subseqs)
   )
  (vec (for [x valid-subseqs]
    (subs digits 0 x) 
    ))
  )

(defn check-if-num-madeof-repeating-digits
  [num]
  (let [digit_groups (generate-digit-groups num) 
        ]
    (mapv #(.repeat % (/ (count num) (count %))) digit_groups)
    (.contains (mapv #(.replaceAll num % "") digit_groups) "") 
    )
  )
;;;;; end part 2

;; Actual implementation

(def rangevecs (loop [r formatted_input
       vrangevals []]
  (if (empty? r) 
    vrangevals 
    (recur (subvec r 1) 
           (conj vrangevals (update
                             (->>
                              (str/split (str/trim (first r)) #"-")
                              (mapv parse-long)
                              )
                             1 inc))) 
    ) 
 ))
(def invalid-ids (loop [rv rangevecs
       ivids []]
  (if (empty? rv)
    ivids
    (recur (rest rv) (vec (concat ivids (->>
                      (range ((first rv) 0) ((first rv) 1))
                      (mapv str)
                      (filter (fn [x] (let [length (count x)
                                            [firsth secondh] (split-at (/ length 2) x)]
                                        (and (= 0 (mod length 2)) (= firsth secondh)))))
                      (mapv parse-long) 
                      )))) 
    )
  ))
(apply + invalid-ids)
;;;
;;; Implementaion 2
(def rangevecs (loop [r formatted_input
                      vrangevals []]
                 (if (empty? r)
                   vrangevals
                   (recur (subvec r 1)
                          (conj vrangevals (update
                                            (->>
                                             (str/split (str/trim (first r)) #"-")
                                             (mapv parse-long))
                                            1 inc))))))
(def invalid-ids (loop [rv rangevecs
                        ivids []]
                   (if (empty? rv)
                     ivids
                     (recur (rest rv) (vec (concat ivids (->>
                                                          (range ((first rv) 0) ((first rv) 1))
                                                          (mapv str)
                                                          (filter check-if-num-madeof-repeating-digits)
                                                          (mapv parse-long))))))))
(apply + invalid-ids)
;;;;;