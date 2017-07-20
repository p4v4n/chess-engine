(ns chess-engine-clj.searchev
  (:require [chess-engine-clj.eval :as eval]))

(defn make-move [board-pos [first-id second-id]]
    (let [moving-piece (get-in board-pos first-id)]
        (-> board-pos
            (assoc-in first-id \-)
            (assoc-in second-id moving-piece))))

(defn best-move [current-board valid-move-list]
  (->> valid-move-list
       (map #(make-move current-board %))
       (map eval/eval-position)
       (map vector valid-move-list)
       (sort-by second)
       last
       first))
