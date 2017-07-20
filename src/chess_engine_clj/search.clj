(ns chess-engine-clj.search
  (:require [chess-engine-clj.board :as board]))

(defn pick-best-move [current-board valid-move-list eval-function]
  (->> valid-move-list
       (map #(board/board-pos-after-move current-board %))
       (map eval-function)
       (map vector valid-move-list)
       (sort-by second)
       last
       first))
