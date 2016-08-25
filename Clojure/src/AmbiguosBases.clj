


(defn generate-character [x]
  (if (>= x 10))
    (char (+ (- x 9) 32))
    x))

(defn orders [x]
  (loop [chars (char-array x)
         a_split 1
         combinations []]
    (if (= (count chars) 1)
      combinations
      (recur chars (take a_split chars))
      )))

; (defn convert [value base]
;   (let [valu (char-array value)]
;     ))