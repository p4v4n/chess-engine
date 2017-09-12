(ns chess-engine-clj.board
    (:require [clojure.string :as string]
              [chess-engine-clj.evaluation :as evaluation]))

(def initial-board [[\r \n \b \q \k \b \n \r]
                    [\p \p \p \p \p \p \p \p]
                    [\- \- \- \- \- \- \- \-]
                    [\- \- \- \- \- \- \- \-]
                    [\- \- \- \- \- \- \- \-]
                    [\- \- \- \- \- \- \- \-]
                    [\P \P \P \P \P \P \P \P]
                    [\R \N \B \Q \K \B \N \R]])

(def initial-board-state {:board initial-board
                          :turn :white
                          :moves-cnt 0
                          :eval 0
                          :game-state :in-progress
                          :game-pgn []
                          :white-can-castle-ks true
                          :white-can-castle-qs true
                          :black-can-castle-ks true
                          :black-can-castle-qs true})

(def uni-pieces {\R "♜", \N "♞", \B "♝", \Q "♛", \K "♚", \P "♟",
                 \r "♖", \n "♘", \b "♗", \q "♕", \k "♔", \p "♙", \- "·"})

;;-----Printing the Board at Command Line-----

(defn line-convert [l-vec]
    (->> (map uni-pieces l-vec)
         (map #(str % " "))
         string/join))

(defn pretty-print [board-state]
    (->> (:board board-state)
         (map line-convert)
         (map #(str (- 8 %1) %2) (range 8))
         (string/join "\n")
         (#(str % "\n a b c d e f g h\n\n"))))

;;---------------Game Play Helpers---------------

(def next-color-map {:white :black :black :white})

(defn parse-square [square-id]
    (let [[fil-id rank] [(subs square-id 0 1) (Integer/parseInt (subs square-id 1))]
          fil (->> [fil-id "a"]
                   (map #(int (.charAt % 0)))
                   (reduce -))]
        [(- 8 rank) fil]))

(defn parse-movestr [move-str]
  (let [[full-move s1 s2] (re-find #"^([a-h][1-8])([a-h][1-8])$" move-str)]
    (vector (parse-square s1) (parse-square s2))))

(defn board-pos-after-normal-move [board-pos move-map]
    (let [move-str (:move move-map)
          [first-id second-id] (parse-movestr move-str)
          moving-piece (get-in board-pos first-id)]
        (-> board-pos
            (assoc-in first-id \-)
            (assoc-in second-id moving-piece))))

(defn board-pos-after-promotion [board-pos move-map]
  (let [move-str (:move move-map)
        [first-id second-id] (parse-movestr move-str)
        target-piece (last move-str)]
    (->> (subs move-str 0 4)
         (assoc move-map :move)
         (board-pos-after-normal-move board-pos)
         (assoc-in second-id target-piece))))

(defn board-pos-after-castling [board-pos move-map turn]
  (let [row-to-modify ({:white 7 :black 0} turn)
        ks-formation (if (= turn :white) [\- \R \K \-] [\- \r \k \-])
        qs-formation (if (= turn :white) [\- \- \K \R \-] [\- \- \k \r \-])
        current-row (board-pos row-to-modify)
        modified-row (vec (if (= (:move move-map) "o-o")
                              (concat (take 4 current-row) ks-formation)
                              (concat qs-formation (drop 5 current-row))))]
    (-> board-pos
        (assoc row-to-modify modified-row))))

(defn board-pos-after-move [board-pos move-map turn]
  (case (:piece-type move-map)
    \c (board-pos-after-castling board-pos move-map turn)
    \o (board-pos-after-promotion board-pos move-map)
    (board-pos-after-normal-move board-pos move-map)))

(defn make-move [board-state move-map]
    (let [next-pos (board-pos-after-move (:board board-state) 
                                         move-map 
                                         (:turn board-state))]
        (-> board-state
        (assoc :board next-pos)
        (assoc :turn (next-color-map (:turn board-state)))
        (assoc :eval (evaluation/eval-position next-pos))
        (update-in [:game-pgn] conj move-map))))

(defn both-kings-alive? [board-state]
    (< -10000 (:eval board-state) 10000))

(defn readable-game-score [board-state]
  (->> (:game-pgn board-state)
       (map :move)
       (partition 2 2 nil)
       (map #(str (first %) " " (second %)))
       (map vector (range 1 (count (:game-pgn board-state))))
       (map #(str (first %) "." (second %)))
       (string/join " ")))

(defn replay-game [board-state]
  (let [move-li (:game-pgn board-state)]
    (loop [moves move-li curr-board-state initial-board-state move-no 1]
      (if (empty? moves)
          (println "---------")
          (let [[current-move-map & rest-moves] moves
                turn (:turn board-state)
                next-board-state (make-move curr-board-state current-move-map)]
            (println move-no turn "moves:" current-move-map "\n")
            (println (pretty-print next-board-state))
            (Thread/sleep 1000)
            (if (= turn "white")
                (recur rest-moves next-board-state move-no)
                (recur rest-moves next-board-state (inc move-no))))))))

(defn end-of-game-action [board-state]
    (if (> (:eval board-state) 0)
            (println "White Won")
            (println "Black Won"))
    (println "Game-Score:") 
    (println (readable-game-score board-state))
    (println "Want to replay the game?(y/n)")
    (let [reply (read-line)]
      (if (= reply "y")
          (replay-game board-state))))
