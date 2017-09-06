(ns chess-engine-clj.core
    (:require [clojure.string :as string]
              [chess-engine-clj.board :as board]
              [chess-engine-clj.movegen :as movegen]
              [chess-engine-clj.evaluation :as evaluation]
              [chess-engine-clj.search :as search])
  (:gen-class))

(defn user-move [board-state]
  (println "User Move: ")
    (let [user-choice (string/trim (read-line))
          color (:turn board-state)
          current-board (:board board-state)
          valid-move-list (movegen/valid-move-list current-board (keyword color))]
        (if (and (board/is-valid-move? user-choice)
                 (contains? (set valid-move-list) user-choice))
            (do (println (str "User Move for " color " : " user-choice "\n"))
                (board/make-move board-state user-choice))          
            (do (println "Please enter a valid move like b1c3")
                (user-move board-state)))))

(defn engine-move [board-state]
    (let [current-board (:board board-state)
          color (:turn board-state)
          valid-move-list (movegen/valid-move-list current-board (keyword color))
          engine-choice (search/pick-best-move current-board color 3)]
        (println (str "Engine Move for " color " : " engine-choice "\n"))
        (board/make-move board-state engine-choice)))

(def player-to-move-fn {"user" user-move "engine" engine-move})

(defn update-board-state[board-state player]
    ((player-to-move-fn player) board-state))

(defn -main [player1 player2]
  (loop [curr-board-state @board/initial-board-state curr-player player1]
    (println (board/pretty-print curr-board-state))
    (if (not (board/both-kings-alive? curr-board-state))
        (board/end-of-game-action curr-board-state)
        (recur (update-board-state curr-board-state curr-player) 
               ((zipmap [player1 player2] [player2 player1]) curr-player)))))
