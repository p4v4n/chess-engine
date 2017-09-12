(ns chess-engine-clj.check
  (:require [chess-engine-clj.board :as board]
            [chess-engine-clj.movegen :as movegen]))

;;To find the list of squares controlled by a side
;-----Knight---------

(defn knight-control-vec [board-vec color curr-locn]
    (->> movegen/knight-moves
         (map #(mapv + curr-locn %))
         (filter movegen/inside-the-board?)))

;------King---------

(defn king-control-vec [board-vec color curr-locn]
    (->> movegen/king-basic-moves
        (map #(mapv + curr-locn %))
        (filter movegen/inside-the-board?)))

;--------Pawn----------

(defn pawn-control-vec [board-vec color curr-locn]
    (->> (movegen/pawn-capture-moves color)
         (map #(mapv + curr-locn %))
         (filter movegen/inside-the-board?)))

;----------Queen+Rook+Bishop---------

(defn long-range-control-single-dirn [board-vec color curr-locn step]
    (->> (range 1 8)
         (map #(mapv (partial * %) step))
         (map #(mapv + curr-locn %))
         (filter movegen/inside-the-board?)
         (movegen/take-until #((movegen/is-enemy-piece? color) board-vec %))
         (movegen/take-until #(not ((movegen/is-friendly-piece? color) board-vec %)))))

(defn long-range-control-all-dirn [board-vec color curr-locn dirn-vec]
    (->> dirn-vec
         (map #(long-range-control-single-dirn board-vec color curr-locn %))
         (apply concat)))

(def queen-control-vec #(long-range-control-all-dirn %1 %2 %3 movegen/all-directions))
(def rook-control-vec #(long-range-control-all-dirn %1 %2 %3 movegen/rook-directions))
(def bishop-control-vec #(long-range-control-all-dirn %1 %2 %3 movegen/bishop-directions))

;---------

(def piece-control-function-map {"n" knight-control-vec "k" king-control-vec 
                                 "p" pawn-control-vec   "q" queen-control-vec 
                                 "r" rook-control-vec   "b" bishop-control-vec})


(defn control-square-list [board-vec color]
    (->> ((movegen/piece-location-fn color) board-vec)
         (mapv #((piece-control-function-map (-> (get-in board-vec %)
                                                 (clojure.string/lower-case))) 
                 board-vec color %))
         (apply concat)
         set))

;;--Filtering illegal moves

(defn king-location [board-vec color]
  (first 
    (filter (comp #(= % (if (= color :white) \K \k))
                #(get-in board-vec %))
           (for [x (range 8)
                 y (range 8)]
                [x y]))))

(defn is-king-in-check? [board-vec color]
  (contains? (control-square-list board-vec (board/next-color-map color))
             (king-location board-vec color)))

(defn is-illegal-move? [board-state move-str]
  (let [curr-board (:board board-state)
        color-to-move (:turn board-state)
        next-board (board/board-pos-after-move curr-board move-str)]
    (is-king-in-check? next-board color-to-move)))

(defn legal-move-list [board-state]
  (->> (movegen/valid-move-list board-state)
       (remove #(is-illegal-move? board-state %))))
