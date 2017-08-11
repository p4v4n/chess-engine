(ns chess-engine-clj.search
  (:require [chess-engine-clj.board :as board]
            [chess-engine-clj.movegen :as movegen]
            [chess-engine-clj.evaluation :as evaluation]))

;;Mini-Max Algorithm
(defn evaluation-at-depth [current-board depth color-to-max]
  (if (= depth 0)
      (evaluation/eval-position2 current-board)
      (let [v-li (movegen/valid-move-list current-board color-to-max)]
        (->> v-li
             (map #(board/board-pos-after-move current-board %))
             (map #(evaluation-at-depth % (dec depth) ({:white :black :black :white} color-to-max)))
             (apply (if (= color-to-max :white) max min))))))

(defn pick-best-move [current-board color eval-function depth]
  (let [valid-move-list (movegen/valid-move-list current-board (keyword color))]
  (->> valid-move-list
       (map #(board/board-pos-after-move current-board %))
       (pmap #(eval-function % (dec depth) ({:white :black :black :white} (keyword color))))
       (map vector valid-move-list)
       (sort-by second)
       ((if (= color "white") last first))
       first)))
