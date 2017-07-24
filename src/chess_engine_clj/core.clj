(ns chess-engine-clj.core
    (:require [clojure.string :as string]
              [chess-engine-clj.board :as board]
              [chess-engine-clj.movegen :as movegen]
              [chess-engine-clj.eval :as eval]
              [chess-engine-clj.search :as search])
  (:gen-class))

(defn user-move [color]
  (println "User Move: ")
    (let [user-choice (string/trim (read-line))]
        (if (board/is-valid-move? user-choice)
            (do (println (str "User Move for " color " : " user-choice "\n"))
                (board/make-move user-choice))          
            (println "Please enter a valid move like b1c3"))))

(defn engine-move [color]
    (let [current-board (:board @board/board-state)
          valid-move-list (movegen/valid-move-list current-board (keyword color))
          engine-choice (search/pick-best-move current-board color valid-move-list eval/eval-position2)]
        (Thread/sleep 1000)
        (println (str "Engine Move for " color " : " engine-move "\n"))
        (board/make-move engine-choice)))

(def player-to-move-fn {"user" user-move "engine" engine-move})

(defn game-play [player]
    ((player-to-move-fn player) (:turn @board/board-state)))

(defn -main [player1 player2]
    (while (board/both-kings-alive?)
        (game-play player1)
        (game-play player2))
    (board/end-of-game-action))
