(ns priority-queue)

(defrecord Node [value left right])

(defn heap-insert [tree x]
  (cond
    (<= x (-> tree :value)) (if (-> tree :left nil?)
                                (assoc tree :left (Node. x nil nil))
                                (assoc tree :left (heap-insert (:left tree) x)))
    (> x (-> tree :value)) (if (-> tree :right nil?)
                               (assoc tree :right (Node. x nil nil))
                               (assoc tree :right (heap-insert (:right tree) x)))))


(defn heapsort [tree]
  (letfn [(decider [x non-nil-side other-side]
                   (if (-> x non-nil-side nil? not)
                       (let [sorted-branch (heapsort (non-nil-side x))]
                         (cond
                           (nil? sorted-branch) x
                           (<= (:value x) (:value sorted-branch)) (let [swapped (assoc 
                                                                                 (assoc 
                                                                                   (assoc x :value (:value sorted-branch)) 
                                                                                   non-nil-side 
                                                                                   (assoc sorted-branch :value (:value x))) 
                                                                                 other-side (-> x other-side heapsort))]
                                                                          (assoc (assoc swapped :left (heapsort (:left swapped))) :right (heapsort (:right swapped))))
                           :else (assoc (assoc x non-nil-side sorted-branch) other-side (-> x other-side heapsort)) ))
                       x))]
    (decider (decider tree :left :right) :right :left)))



(def root (Node. 2 nil nil))

(def my-tree (reduce (fn [x y] (heap-insert x y)) root [6 5 43 4 56 7 4 3 1 2 11 9]))

(heapsort my-tree)