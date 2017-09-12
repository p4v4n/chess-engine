(ns chess-engine-clj.search
  (:require [chess-engine-clj.board :as board]
            [chess-engine-clj.movegen :as movegen]
            [chess-engine-clj.evaluation :as evaluation]))

;;Mini-Max Algorithm
(defn evaluation-at-depth [current-board depth color-to-max]
  (if (= depth 0)
      (evaluation/eval-position current-board)
      (let [v-li (movegen/valid-move-list current-board color-to-max)]
        (->> v-li
             (map #(board/board-pos-after-move current-board % color-to-max))
             (map #(evaluation-at-depth % (dec depth) (board/next-color-map color-to-max)))
             (apply (if (= color-to-max :white) max min))))))

;;-----------
;;With alpha-beta pruning
(defn alpha-beta [board-state depth alpha beta]
  (let [current-eval (:eval board-state)
        color-to-max (:turn board-state)]
  (if (or (zero? depth) (not (< -10000 current-eval 10000)))
      current-eval
      (let [valid-move-list (movegen/valid-move-list board-state)]
        (if (= color-to-max :white)
            (loop [move-list valid-move-list a alpha b beta]
              (if (or (empty? move-list) (>= a b))
                  a
                  (let [a-new (alpha-beta (board/make-move board-state 
                                                          (first move-list)) 
                                          (dec depth) a b)]
                  (recur (rest move-list) (max a a-new) b))))
            (loop [move-list valid-move-list a alpha b beta]
              (if (or (empty? move-list) (>= a b))
                  b
                  (let [b-new (alpha-beta (board/make-move board-state
                                                           (first move-list)) 
                                          (dec depth) a b)]
                  (recur (rest move-list) a (min b b-new))))))))))

(defn pick-best-move [board-state depth]
  (let [current-board (:board board-state)
        color (:turn board-state)
        valid-move-list (movegen/valid-move-list board-state)]
    (->> valid-move-list
         (map #(board/make-move board-state %))
         (map #(alpha-beta % (dec depth) -100000 100000))
         (map vector valid-move-list)
         (sort-by second)
         ((if (= color :white) last first))
         first)))
