(ns chess-engine-clj.engine
    (:require [clojure.string :as string]
              [chess-engine-clj.movegen :as movegen]))

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

;;---------------Game Play---------------

(defn parse-square [square-id]
    (let [[fil-id rank] [(subs square-id 0 1) (Integer/parseInt (subs square-id 1))]
          fil (->> [fil-id "a"]
                   (map #(int (.charAt % 0)))
                   (reduce -))]
        [(- 8 rank) fil]))

(defn make-move [board-pos [first-id second-id]]
    (let [moving-piece (get-in board-pos first-id)]
        (-> board-pos
            (assoc-in first-id \-)
            (assoc-in second-id moving-piece))))

(defn read-move [move]
    (let [[full-move s1 s2] (re-find #"^([a-h][1-8])([a-h][1-8])$" move)
          next-pos (make-move (:board @board-state) [(parse-square s1) (parse-square s2)])]
      (swap! board-state assoc :board next-pos)
      (println (pretty-print))))

(defn game-play []
    (println (pretty-print))
    (println "Your Move: ")
    (let [user-move (string/trim (read-line))]
        (read-move user-move))
    (Thread/sleep 2000)
    (let [engine-move (movegen/engine-move-pick (:board @board-state))]
        (println (str "My Move: " engine-move "\n"))
        (read-move engine-move)))
