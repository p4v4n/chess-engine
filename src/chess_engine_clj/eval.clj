(ns chess-engine-clj.eval)

;;Evaluation for black-side

(def piece-value {\p 1 \n 3 \b 3 \r 5 \q 9 \k 1000
	              \P -1 \N -3 \B -3 \R -5 \Q -9 \K -1000 \- 0})

(defn eval-position [board-vec]
  (->> (flatten board-vec)
       (map piece-value)
       (reduce +)))
