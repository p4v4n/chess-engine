(ns chess-engine-clj.core
    (:require [clojure.string :as string]
              [chess-engine-clj.board :as board]
              [chess-engine-clj.movegen :as movegen]
              [chess-engine-clj.eval :as eval]
              [chess-engine-clj.search :as search])
  (:gen-class))

(defn white-move []
  (println "Your Move: ")
    (let [user-move (string/trim (read-line))]
        (if (board/is-valid-move? user-move)
            (board/make-move user-move)
            (println "Please enter a valid move like b1c3"))))

(defn black-move []
    (let [current-board (:board @board/board-state)
          valid-move-list (movegen/engine-valid-move-list current-board)
          engine-move (search/pick-best-move current-board valid-move-list eval/eval-position2)]
        (Thread/sleep 1000)
        (println (str "My Move: " engine-move "\n"))
        (board/make-move engine-move)))

(def player-to-move-fn {"white" white-move "black" black-move})

(defn game-play []
    ((player-to-move-fn (:turn @board/board-state))))

(defn -main []
    (while (board/both-kings-alive?)
        (game-play))
    (board/end-of-game-action))
