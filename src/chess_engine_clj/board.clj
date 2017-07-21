(ns chess-engine-clj.board
    (:require [clojure.string :as string]
              [chess-engine-clj.eval :as eval]))

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
                        :eval 0
                        :game-state :in-progress
                        :white-can-castle-ks true
                        :white-can-castle-qs true
                        :black-can-castle-ks true
                        :black-can-castle-qs true}))

(def uni-pieces {\R "♜", \N "♞", \B "♝", \Q "♛", \K "♚", \P "♟",
                 \r "♖", \n "♘", \b "♗", \q "♕", \k "♔", \p "♙", \- "·"})

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

;;---------------Game Play Helpers---------------

(defn parse-square [square-id]
    (let [[fil-id rank] [(subs square-id 0 1) (Integer/parseInt (subs square-id 1))]
          fil (->> [fil-id "a"]
                   (map #(int (.charAt % 0)))
                   (reduce -))]
        [(- 8 rank) fil]))

(defn parse-movestr [move-str]
  (let [[full-move s1 s2] (re-find #"^([a-h][1-8])([a-h][1-8])$" move-str)]
    (vector (parse-square s1) (parse-square s2))))

(defn board-pos-after-move [board-pos move-str]
    (let [[first-id second-id] (parse-movestr move-str)
          moving-piece (get-in board-pos first-id)]
        (-> board-pos
            (assoc-in first-id \-)
            (assoc-in second-id moving-piece))))

(defn make-move [move-str]
    (let [next-pos (board-pos-after-move (:board @board-state) move-str)]
        (swap! board-state assoc :board next-pos)
        (swap! board-state assoc :turn ({"white" "black" "black" "white"} (:turn @board-state)))
        (swap! board-state assoc :eval (eval/eval-position next-pos))
        (println (pretty-print))))

(defn both-kings-alive? []
    (< -500 (:eval @board-state) 500))

(defn end-of-game-action []
    (if (> (:eval @board-state) 0)
        (do (swap! board-state assoc :game-state :black-won)
            (println "Checkmate!"))
        (do (swap! board-state assoc :game-state :white-won)
            (println "You Win"))))
