(ns chess-engine-clj.core
    (:require [clojure.string :as string]
              [chess-engine-clj.board :as board]
              [chess-engine-clj.movegen :as movegen]
              [chess-engine-clj.eval :as eval]
              [chess-engine-clj.search :as search])
  (:gen-class))

(defn game-play []
    (println "Your Move: ")
    (let [user-move (string/trim (read-line))]
        (board/make-move user-move))
    (Thread/sleep 2000)
    (let [engine-move (movegen/engine-move-pick (:board @board/board-state))]
        (println (str "My Move: " engine-move "\n"))
        (board/make-move engine-move)))

(defn -main []
    (while true
        (game-play)))
