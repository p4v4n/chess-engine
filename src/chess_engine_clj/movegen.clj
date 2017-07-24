(ns chess-engine-clj.movegen)

;;-----Generating Moves ----------

;------Directions----------

(def dir-up [-1 0])
(def dir-down [+1 0])
(def dir-left [0 -1])
(def dir-right [0 +1])
(def dir-up-left [-1 -1])
(def dir-up-right [-1 +1])
(def dir-down-left [+1 -1])
(def dir-down-right [+1 +1])

(def rook-directions [dir-up dir-down dir-left dir-right])
(def bishop-directions [dir-up-left dir-up-right dir-down-left dir-down-right])
(def all-directions (concat rook-directions bishop-directions))
(def knight-moves [[-2 -1] [-1 -2] [+1 -2] [+2 -1]
                   [-2 +1] [-1 +2] [+1 +2] [+2 +1]])
(def king-basic-moves all-directions)
(def pawn-basic-moves {:white [[-1 0] [-2 0]] :black [[+1 0] [+2 0]]})
(def pawn-capture-moves {:white [[-1 +1] [-1 -1]] :black [[+1 +1] [+1 -1]]})
(def queen-moves all-directions)


(defn black-piece-location [board-vec]
  (filter (comp #(Character/isLowerCase %)
                #(get-in board-vec %))
           (for [x (range 8)
                 y (range 8)]
                [x y])))

(defn white-piece-location [board-vec]
  (filter (comp #(Character/isUpperCase %)
                #(get-in board-vec %))
           (for [x (range 8)
                 y (range 8)]
                [x y])))

(defn inside-the-board? [[i j]]
    (and (<= 0 i 7) (<= 0 j 7)))

(defn is-black-piece? [board-vec [i j]]
    (->> (get-in board-vec [i j])
         (#(Character/isLowerCase %))))

(defn is-white-piece? [board-vec [i j]]
    (->> (get-in board-vec [i j])
         (#(Character/isUpperCase %))))

(defn is-friendly-piece? [color]
  (case color
    :white is-white-piece?
    :black is-black-piece?))

(defn is-enemy-piece? [color]
  (case color
    :white is-black-piece?
    :black is-white-piece?))

(defn empty-square? [board-vec [i j]]
    (->> (get-in board-vec [i j])
         (= \-)))

(defn take-until [pred coll]
    (lazy-seq
        (when-let [s (seq coll)]
            (if (pred (first s))
                (cons (first s) nil)
                (cons (first s) (take-until pred (rest s)))))))

(defn id-to-square [[i j]]
    (let [fil (char (+ (int \a) j))
          rank (- 8 i)]
          (str fil rank)))

(defn id-to-move-str [[id1 id2]]
    (->> [id1 id2]
         (map id-to-square)
         (apply str)))

;-----Knight---------

(defn knight-moves-vec [board-vec color curr-locn]
    (->> knight-moves
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (remove #((is-friendly-piece? color) board-vec %))
         (mapv #(vector curr-locn %))))

;------King---------

(defn king-basic-moves-vec [board-vec color curr-locn]
    (->> king-basic-moves
        (map #(mapv + curr-locn %))
        (filter inside-the-board?)
        (remove #((is-friendly-piece? color) board-vec %))
        (mapv #(vector curr-locn %))))

;--------Pawn----------

(defn pawn-basic-moves-vec [board-vec color curr-locn]
   (->> (if (= (if (= color :white) 6 1) (first curr-locn)) 
            (pawn-basic-moves color)
            (drop-last (pawn-basic-moves color)))
        (map #(mapv + curr-locn %))
        (take-while #(empty-square? board-vec %))
        (mapv #(vector curr-locn %))))

(defn pawn-capture-moves-vec [board-vec color curr-locn]
    (->> (pawn-capture-moves color)
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (filter #((is-enemy-piece? color) board-vec %))
         (mapv #(vector curr-locn %))))

(defn pawn-moves-vec [board-vec color curr-locn]
    (concat (pawn-basic-moves-vec board-vec color curr-locn)
            (pawn-capture-moves-vec board-vec color curr-locn)))

;----------Queen+Rook+Bishop---------

(defn long-range-moves-single-dirn [board-vec color curr-locn step]
    (->> (range 1 8)
         (map #(mapv (partial * %) step))
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (take-until #((is-enemy-piece? color) board-vec %))
         (take-while #(not ((is-friendly-piece? color) board-vec %)))))

(defn long-range-moves-all-dirn [board-vec color curr-locn dirn-vec]
    (->> dirn-vec
         (map #(long-range-moves-single-dirn board-vec color curr-locn %))
         (apply concat)
         (mapv #(vector curr-locn %))))

(def queen-moves-vec #(long-range-moves-all-dirn %1 %2 %3 all-directions))
(def rook-moves-vec #(long-range-moves-all-dirn %1 %2 %3 rook-directions))
(def bishop-moves-vec #(long-range-moves-all-dirn %1 %2 %3 bishop-directions))

;---------

(def piece-function-map {\n knight-moves-vec \k king-basic-moves-vec \p pawn-moves-vec
                         \q queen-moves-vec \r rook-moves-vec \b bishop-moves-vec
                         \N knight-moves-vec \K king-basic-moves-vec \P pawn-moves-vec
                         \Q queen-moves-vec \R rook-moves-vec \B bishop-moves-vec})

(defn piece-location-fn [color]
  (case color
    :white white-piece-location
    :black black-piece-location))

(defn valid-move-list [board-vec color]
    (->> ((piece-location-fn color) board-vec)
         (mapv #((piece-function-map (get-in board-vec %)) board-vec color %))
         (apply concat)
         (mapv id-to-move-str)))
