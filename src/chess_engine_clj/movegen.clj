(ns chess-engine-clj.movegen
  (:require [clojure.string :as string]
            [chess-engine-clj.board :as board]))

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
         (map #(vector curr-locn %))
         (mapv #(hash-map :move % :piece-type \n))))

;------King---------

(defn king-basic-moves-vec [board-vec color curr-locn]
    (->> king-basic-moves
        (map #(mapv + curr-locn %))
        (filter inside-the-board?)
        (remove #((is-friendly-piece? color) board-vec %))
        (map #(vector curr-locn %))
        (mapv #(hash-map :move % :piece-type \k))))

;--------Pawn----------

(defn pawn-basic-moves-vec [board-vec color curr-locn]
   (->> (if (= (if (= color :white) 6 1) (first curr-locn)) 
            (pawn-basic-moves color)
            (drop-last (pawn-basic-moves color)))
        (map #(mapv + curr-locn %))
        (take-while #(empty-square? board-vec %))
        (map #(vector curr-locn %))
        (mapv #(hash-map :move % :piece-type \p))))

(defn pawn-capture-moves-vec [board-vec color curr-locn]
    (->> (pawn-capture-moves color)
         (map #(mapv + curr-locn %))
         (filter inside-the-board?)
         (filter #((is-enemy-piece? color) board-vec %))
         (map #(vector curr-locn %))
         (mapv #(hash-map :move % :piece-type \p))))

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

(defn long-range-moves-all-dirn [board-vec color curr-locn dirn-vec piece-type]
    (->> dirn-vec
         (map #(long-range-moves-single-dirn board-vec color curr-locn %))
         (apply concat)
         (map #(vector curr-locn %))
         (mapv #(hash-map :move % :piece-type piece-type))))

(def queen-moves-vec #(long-range-moves-all-dirn %1 %2 %3 all-directions \q))
(def rook-moves-vec #(long-range-moves-all-dirn %1 %2 %3 rook-directions \r))
(def bishop-moves-vec #(long-range-moves-all-dirn %1 %2 %3 bishop-directions \b))

;---------

(def piece-function-map {"n" knight-moves-vec "k" king-basic-moves-vec 
                         "p" pawn-moves-vec "q" queen-moves-vec 
                         "r" rook-moves-vec "b" bishop-moves-vec})

(defn piece-location-fn [color]
  (case color
    :white white-piece-location
    :black black-piece-location))

(defn normal-move-list [[board-vec color]]
    (->> ((piece-location-fn color) board-vec)
         (map #((piece-function-map (-> (get-in board-vec %)
                                        (clojure.string/lower-case))) 
              board-vec color %))
         (apply concat)
         (mapv #(update-in % [:move] id-to-move-str))))

;--------Special Moves

;;---castling

(defn ks-castling-move [[board-vec color]]
  (->> ({:black 0 :white 7} color)
       (get board-vec)
       (drop 4)
       (string/join "")
       string/lower-case
       (= "k--r")
       (#(if % {:move "o-o" :piece-type \c}))))

(defn qs-castling-move [[board-vec color]]
  (->> ({:black 0 :white 7} color)
       (get board-vec)
       (take 5)
       (string/join "")
       string/lower-case
       (= "r---k")
       (#(if % {:move "o-o-o" :piece-type \c}))))

(defn castling-moves-vec [[board-vec color]]
  (->> [board-vec color]
       ((juxt ks-castling-move qs-castling-move))
       (remove nil?)))

;;---enpassant
(defn enpassant-move-list [board-state]
  (let [curr-board (:board board-state)
        turn (:turn board-state)
        last-move-map (->> (:game-pgn board-state)
                           last)
        last-piece (:piece-type last-move-map)]
      (if (= \p last-piece)  
        (let [last-move (:move last-move-map)
        [first-id second-id] (if last-move-map 
                                 (board/parse-movestr last-move))
        expected-rows (if (= :white turn)
                          [1 3]
                          [6 4])
        actual-rows [(first first-id) (first second-id)]]
    (if (= actual-rows expected-rows)
        (let [opp-pawn (if (= :white turn) \P \p)
              side-squares (->> [-1 +1]
                                (map #(+ % (second second-id)))
                                (mapv #(vector (first second-id) %))
                                (filter #(= opp-pawn (get-in curr-board %))))]      
          (if (not-empty side-squares)
              (->> side-squares
                   (map #(vector % [(+ (first first-id) 
                                       ({:white 1 :black -1} turn)) 
                                    (second first-id)]))
                   (map id-to-move-str)
                   (map #(hash-map :move % :piece-type \e))))))))))

;;---pawn-promotion
;turn pawn-move on 7th rank to pawn-promotion move

(defn turn-to-promotion [color move-map]
  (if (and (= \p (:piece-type move-map))
           (= ({:white \7 :black \2} color) 
              (get-in move-map [:move 1])))
      (->> [\q \r \n \b]
           (map #(str (:move move-map) \= %))
           (map #(hash-map :move % :piece-type \o)))
      move-map))

(defn valid-move-list [board-state]
  (let [color (:turn board-state)
        board-vec (:board board-state)]
  (->> [board-vec color]
       ((juxt normal-move-list castling-moves-vec))
       (apply concat)
       (concat (enpassant-move-list board-state))
       (mapv #(turn-to-promotion color %))
       flatten)))
