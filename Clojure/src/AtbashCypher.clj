(ns atbashcypher 
  (:require [clojure.string :as string]))

(defn get-coded-char [x]
  (if (> (int x) 90)
      (cond
        (> (int x) 110) (- 110 (- (inc (int x)) 110))
        :else (+ 110 (- 110 (inc (int x)))))  
      (cond
        (> (int x) 77) (- 77 (- (int x) 77))
        :else (+ 77 (- 77 (int x))))))

(defn convert-string [x]
  (let [char-set (char-array x)]
    (string/join (map char (map #(get-coded-char %) char-set)))))
   