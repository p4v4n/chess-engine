(ns chess-engine-clj.evaluation)

;; Evaluation from white-side
;; Using Tomasz Michniewski's  Simplified evaluation function

(def piece-value {\K  20000 \Q   900  \R  500
                  \B  300 \N  300  \P  100
                  \k -20000 \q  -900  \r -500
                  \b -300 \n -300  \p -100
                  \- 0})

(def white-bonus-eval-tables
  {\P
   [[  0   0   0   0   0   0   0   0 ]
    [ 50  50  50  50  50  50  50  50 ]
    [ 10  10  20  30  30  20  10  10 ]
    [  5   5  10  25  25  10   5   5 ]
    [  0   0   0  20  20   0   0   0 ]
    [  5  -5 -10   0   0 -10  -5   5 ]
    [  5  10  10 -20 -20  10  10   5 ]
    [  0   0   0   0   0   0   0   0 ]]

   \N
   [[-50 -40 -30 -30 -30 -30 -40 -50 ]
    [-40 -20   0   0   0   0 -20 -40 ]
    [-30   0  10  15  15  10   0 -30 ]
    [-30   5  15  20  20  15   5 -30 ]
    [-30   0  15  20  20  15   0 -30 ]
    [-30   5  10  15  15  10   5 -30 ]
    [-40 -20   0   5   5   0 -20 -40 ]
    [-50 -40 -30 -30 -30 -30 -40 -50 ]]

   \B
   [[-20 -10 -10 -10 -10 -10 -10 -20 ]
    [-10   0   0   0   0   0   0 -10 ]
    [-10   0   5  10  10   5   0 -10 ]
    [-10   5   5  10  10   5   5 -10 ]
    [-10   0  10  10  10  10   0 -10 ]
    [-10  10  10  10  10  10  10 -10 ]
    [-10   5   0   0   0   0   5 -10 ]
    [-20 -10 -10 -10 -10 -10 -10 -20 ]]

   \R
   [[  0   0   0   0   0   0   0   0 ]
    [  5  10  10  10  10  10  10   5 ]
    [ -5   0   0   0   0   0   0  -5 ]
    [ -5   0   0   0   0   0   0  -5 ]
    [ -5   0   0   0   0   0   0  -5 ]
    [ -5   0   0   0   0   0   0  -5 ]
    [ -5   0   0   0   0   0   0  -5 ]
    [  0   0   0   5   5   0   0   0 ]]

   \Q
   [[-20 -10 -10  -5  -5 -10 -10 -20 ]
    [-10   0   0   0   0   0   0 -10 ]
    [-10   0   5   5   5   5   0 -10 ]
    [ -5   0   5   5   5   5   0  -5 ]
    [  0   0   5   5   5   5   0  -5 ]
    [-10   5   5   5   5   5   0 -10 ]
    [-10   0   5   0   0   0   0 -10 ]
    [-20 -10 -10  -5  -5 -10 -10 -20 ]]

   \K
   [[-30 -40 -40 -50 -50 -40 -40 -30 ]
    [-30 -40 -40 -50 -50 -40 -40 -30 ]
    [-30 -40 -40 -50 -50 -40 -40 -30 ]
    [-30 -40 -40 -50 -50 -40 -40 -30 ]
    [-20 -30 -30 -40 -40 -30 -30 -20 ]
    [-10 -20 -20 -20 -20 -20 -20 -10 ]
    [ 20  20   0   0   0   0  20  20 ]
    [ 20  30  10   0   0  10  30  20 ]]})

(defn bonus-eval-convert-for-black [[piece-type bonus-eval-table]]
  (vector ((comp first seq clojure.string/lower-case) piece-type) 
          (->> bonus-eval-table
               flatten
               (map -)
               reverse
               (partition 8)
               (map vec)
               vec)))

(def black-bonus-eval-tables (->> (into [] white-bonus-eval-tables)
                                  (map bonus-eval-convert-for-black)
                                  (into {})))

(def bonus-eval-tables (merge white-bonus-eval-tables
                              black-bonus-eval-tables))

(defn piece-contribution [[location-ind piece-type]]
  (if (= piece-type \-) 
      0
      (+ (piece-value piece-type) 
         (get-in (bonus-eval-tables piece-type) location-ind))))

(defn eval-position [board-vec]
  (->> (flatten board-vec)
       (map vector (for [x (range 8) y (range 8)] [x y]))
       (map piece-contribution)
       (reduce +)))
