(ns chess-engine-clj.search)

(defn baord-pos-after-move [board-pos [first-id second-id]]
    (let [moving-piece (get-in board-pos first-id)]
        (-> board-pos
            (assoc-in first-id \-)
            (assoc-in second-id moving-piece))))

(defn pick-best-move [current-board valid-move-list eval-function]
  (->> valid-move-list
       (map #(baord-pos-after-move current-board %))
       (map eval-function)
       (map vector valid-move-list)
       (sort-by second)
       last
       first))
