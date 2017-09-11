(ns chess-engine-clj.core
    (:require [clojure.string :as string]
              [chess-engine-clj.board :as board]
              [chess-engine-clj.movegen :as movegen]
              [chess-engine-clj.search :as search]
              [chess-engine-clj.check :as check])
  (:gen-class))

(defn user-move [board-state]
    (let [user-choice (do (print "user move:") (flush) (read-line))
          color (:turn board-state)
          current-board (:board board-state)
          valid-move-list (movegen/valid-move-list current-board color)
          user-choice-map (->> valid-move-list
                               (filter #(= user-choice (:move %)))
                               first)]
        (if user-choice-map
            (do (println (str "user move for " color " : " user-choice "\n"))
                (board/make-move board-state user-choice-map))          
            (do (println "please enter a valid move!")
                (user-move board-state)))))

(defn engine-move [board-state]
    (let [current-board (:board board-state)
          color (:turn board-state)
          valid-move-list (movegen/valid-move-list current-board color)
          engine-choice (search/pick-best-move current-board color 3)]
        (println (str "engine move for " color " : " engine-choice "\n"))
        (board/make-move board-state engine-choice)))

(def player-to-move-fn {"user" user-move "engine" engine-move})

(defn update-board-state[board-state player]
    ((player-to-move-fn player) board-state))

(defn game-play [player1 player2]
  (loop [curr-board-state board/initial-board-state curr-player player1]
    (println (board/pretty-print curr-board-state))
    (if (not (board/both-kings-alive? curr-board-state))
        (board/end-of-game-action curr-board-state)
        (recur (update-board-state curr-board-state curr-player) 
               ((zipmap [player1 player2] [player2 player1]) curr-player)))))

(defn ask-for-new-game [player1 player2]
  (let [response (do (print "want to play another game?(y/n)") (flush) (read-line))]
    (if (= response "y")
        (game-play player2 player1)
        (println "bye for now!"))))

(defn -main [player1 player2]
  (game-play player1 player2)
  (ask-for-new-game player1 player2))
