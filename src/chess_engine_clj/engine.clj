(ns chess-engine-clj.engine
    (:require [clojure.string :as string]))

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

(def uni-pieces {\R "♜", \N "♞", \B "♝", \Q "♛", \K "♚", \P "♟",
                 \r "♖", \n "♘", \b "♗", \q "♕", \k "♔", \p "♙", \- "·"})

;;-----Generating Moves ----------

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

(defn black-piece? [[i j]]
    (->> (get-in @board-state [:board i j])
         (#(Character/isLowerCase %))))

(defn white-piece? [[i j]]
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
         (remove black-piece?)
         (mapv #(vector curr-locn %))))

;------King---------

(defn king-basic-moves-vec [curr-locn]
    (->> king-basic-moves
        (map #(mapv + curr-locn %))
        (filter inside-the-board?)
        (remove black-piece?)
        (mapv #(vector curr-locn %))))

;--------Pawn----------

(defn pawn-basic-moves-vec [curr-locn]
   (->> pawn-basic-moves
        (map #(mapv + curr-locn %))
        (take-while empty-square?)
        (mapv #(vector curr-locn %))))

(defn pawn-capture-moves-vec [curr-locn]
    (->> pawn-capture-moves
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (filter white-piece?)
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
         (take-until white-piece?)
         (take-while #(not (black-piece? %)))))

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

(defn engine-move-pick []
    "Picks a random move from the engine-valid-move-list"
    (->> (count (engine-valid-move-list))
         (rand-int)
         (get (engine-valid-move-list))))

;;-----Printing the Board at Command Line-----

(defn line-convert [l-vec]
    (->> (map uni-pieces l-vec)
         (map #(str % " "))
         string/join))

(defn pretty-print []
    (->> (:board @board-state)
         (map line-convert)
         (map #(str (- 8 %1) %2) (range 8))
         (string/join "\n")
         (#(str % "\n a b c d e f g h\n\n"))))

(println (pretty-print))

;;------------------------------
(defn parse-square [square-id]
    (let [[fil-id rank] [(subs square-id 0 1) (Integer/parseInt (subs square-id 1))]
          fil (->> [fil-id "a"]
                   (map #(int (.charAt % 0)))
                   (reduce -))]
        [(- 8 rank) fil]))

(defn make-move [first-id second-id]
    (let [[i1 j1] first-id
          [i2 j2] second-id
          moving-piece (get-in @board-state [:board i1 j1])]
          (swap! board-state assoc-in [:board i1 j1] \-)
          (swap! board-state assoc-in [:board i2 j2] moving-piece))
    (println (pretty-print)))

(defn read-move [move]
    (let [[full-move s1 s2] (re-find #"^([a-h][1-8])([a-h][1-8])$" move)]
        (make-move (parse-square s1) (parse-square s2))))

(defn game-play []
    (println "Your Move: ")
    (let [user-move (string/trim (read-line))]
      (read-move user-move))
    (Thread/sleep 2000)
    (let [engine-move (engine-move-pick)]
      (println (str "My Move: " engine-move "\n"))
      (read-move engine-move)))
