(ns chess-engine-clj.movegen)

(def initial-board [[\r \n \b \q \k \b \n \r]
                    [\p \p \p \p \p \p \p \p]
                    [\- \- \- \- \- \- \- \-]
                    [\- \- \- \- \- \- \- \-]
                    [\- \- \- \- \- \- \- \-]
                    [\- \- \- \- \- \- \- \-]
                    [\P \P \P \P \P \P \P \P]
                    [\R \N \B \Q \K \B \N \R]])

(def board-state (atom {:board initial-board
                        :turn "white"
                        :moves-cnt 0
                        :white-can-castle-ks true
                        :white-can-castle-qs true
                        :black-can-castle-ks true
                        :black-can-castle-qs true}))

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
(def pawn-basic-moves [[+1 0] [+2 0]])
(def pawn-capture-moves [[+1 +1] [+1 -1]])
(def queen-moves all-directions)


(defn black-piece-location []
  "Returns the indices for location of black pieces"
  (filter (comp #(Character/isLowerCase %)
                #(get-in (:board @board-state) %))
           (for [x (range 8)
                 y (range 8)]
                [x y])))

(defn inside-the-board? [[i j]]
    (and (<= 0 i 7) (<= 0 j 7)))

(defn is-black-piece? [[i j]]
    (->> (get-in @board-state [:board i j])
         (#(Character/isLowerCase %))))

(defn is-white-piece? [[i j]]
    (->> (get-in @board-state [:board i j])
         (#(Character/isUpperCase %))))

(defn empty-square? [[i j]]
    (->> (get-in @board-state [:board i j])
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

(defn knight-moves-vec [curr-locn]
    (->> knight-moves
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (remove is-black-piece?)
         (mapv #(vector curr-locn %))))

;------King---------

(defn king-basic-moves-vec [curr-locn]
    (->> king-basic-moves
        (map #(mapv + curr-locn %))
        (filter inside-the-board?)
        (remove is-black-piece?)
        (mapv #(vector curr-locn %))))

;--------Pawn----------

(defn pawn-basic-moves-vec [curr-locn]
   (->> (if (= 1 (first curr-locn)) 
            pawn-basic-moves 
            (drop-last pawn-basic-moves))
        (map #(mapv + curr-locn %))
        (take-while empty-square?)
        (mapv #(vector curr-locn %))))

(defn pawn-capture-moves-vec [curr-locn]
    (->> pawn-capture-moves
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (filter is-white-piece?)
         (mapv #(vector curr-locn %))))

(defn pawn-moves-vec [curr-locn]
    (concat (pawn-basic-moves-vec curr-locn)
            (pawn-capture-moves-vec curr-locn)))

;----------Queen+Rook+Bishop---------

(defn long-range-moves-single-dirn [curr-locn step]
    (->> (range 1 8)
         (map #(mapv (partial * %) step))
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (take-until is-white-piece?)
         (take-while #(not (is-black-piece? %)))))

(defn long-range-moves-all-dirn [curr-locn dirn-vec]
    (->> dirn-vec
         (map #(long-range-moves-single-dirn curr-locn %))
         (apply concat)
         (mapv #(vector curr-locn %))))

(def queen-moves-vec #(long-range-moves-all-dirn % all-directions))
(def rook-moves-vec #(long-range-moves-all-dirn % rook-directions))
(def bishop-moves-vec #(long-range-moves-all-dirn % bishop-directions))

;---------

(def piece-function-map {\n knight-moves-vec \k king-basic-moves-vec \p pawn-moves-vec
                         \q queen-moves-vec \r rook-moves-vec \b bishop-moves-vec})

(defn engine-valid-move-list []
    (->> (black-piece-location)
         (mapv #((piece-function-map (get-in (:board @board-state) %)) %))
         (apply concat)
         (mapv id-to-move-str)))

(defn engine-move-pick [board-vec]
    "Picks a random move from the engine-valid-move-list"
    (swap! board-state assoc-in [:board] board-vec)
    (->> (count (engine-valid-move-list))
         (rand-int)
         (get (engine-valid-move-list))))
