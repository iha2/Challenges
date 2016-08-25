(ns djikstra)

(def data '([0, 4, 0, 0, 0, 0, 0, 8, 0],
            [4, 0, 8, 0, 0, 0, 0, 11, 0],
            [0, 8, 0, 7, 0, 4, 0, 0, 2],
            [0, 0, 7, 0, 9, 14, 0, 0, 0],
            [0, 0, 0, 9, 0, 10, 0, 0, 0],
            [0, 0, 4, 0, 10, 0, 2, 0, 0],
            [0, 0, 0, 14, 0, 2, 0, 1, 6],
            [8, 11, 0, 0, 0, 0, 1, 0, 7],
            [0, 0, 2, 0, 0, 0, 6, 7, 0]))

(defn min-distance [distances spanning-tree-set]
  (let [key-set (conj (reverse (keys distances)) (keyword "-1"))
        min-num Integer/MAX_VALUE
        dists (merge {:-1 min-num} distances)
        spt-set (merge {:-1 false} spanning-tree-set)]
    (prn dists)
    (reduce
      (fn [x y] (if (and (false? (x spt-set)) (< (y dists) (x dists))) y x)) 
      key-set)))


(defn djikstra [graph n]
  (let [distances (apply merge (map-indexed (fn [x y] {(-> x str keyword) y}) (cons 0 (repeat (dec n) Integer/MAX_VALUE))))
        spanning-tree-set (apply merge (map-indexed (fn [x y] {(-> x str keyword) y}) (repeat n false)))]
    
    (loop [dist distances
           spt-set spanning-tree-set
           counter (- (count dist) 2)]
      (let [mini (min-distance dist spt-set)]
        (let [spt-set-new (assoc spt-set mini true)
              min-adj-vertex (fn [x] (nth (nth graph (-> mini name read-string)) 
                                                     (-> x first name read-string)))]
          (prn "start")
          (prn dist)
          (prn mini)
          (let [new-dists (apply merge 
                                 (map (fn [x]
                                        (prn " ")
                                        (prn x)
                                        (prn {(first x) (+ (mini dist) (min-adj-vertex x))})
                                        (prn {(first x) ((first x) dist)})
                                        (prn " ")
                                        (if (and 
                                             (false? ((first x) spt-set))
                                             (min-adj-vertex x)
                                             (not= (mini dist) Integer/MAX_VALUE)
                                             (< (+ (mini dist)
                                                   (min-adj-vertex x))
                                                ((first x) dist)))
                                            {(first x) (+ (mini dist) (min-adj-vertex x))}
                                            {(first x) ((first x) dist)}))
                                         spt-set-new))]
            (if (zero? counter)
              dist
              (recur new-dists spt-set-new (dec counter)))))))))


; (min-distance (cons 0 (repeat 7 Integer/MAX_VALUE)) (repeat 8 false))