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
(defn alpha-beta [current-board depth alpha beta color-to-max]
  (let [current-eval (evaluation/eval-position current-board)]
  (if (or (zero? depth) (not (< -10000 current-eval 10000)))
      current-eval
      (let [valid-move-list (movegen/valid-move-list current-board color-to-max)]
        (if (= color-to-max :white)
            (loop [move-list valid-move-list a alpha b beta]
              (if (or (empty? move-list) (>= a b))
                  a
                  (let [a-new (alpha-beta (board/board-pos-after-move current-board 
                                                                      (first move-list)
                                                                      color-to-max) 
                                          (dec depth) a b 
                                          (board/next-color-map color-to-max))]
                  (recur (rest move-list) (max a a-new) b))))
            (loop [move-list valid-move-list a alpha b beta]
              (if (or (empty? move-list) (>= a b))
                  b
                  (let [b-new (alpha-beta (board/board-pos-after-move current-board 
                                                                      (first move-list)
                                                                      color-to-max) 
                                          (dec depth) a b 
                                          (board/next-color-map color-to-max))]
                  (recur (rest move-list) a (min b b-new))))))))))

(defn pick-best-move [current-board color depth]
  (let [valid-move-list (movegen/valid-move-list current-board color)]
    (->> valid-move-list
         (map #(board/board-pos-after-move current-board % color))
         (map #(alpha-beta % (dec depth) -100000 100000 (board/next-color-map color)))
         (map vector valid-move-list)
         (sort-by second)
         ((if (= color :white) last first))
         first)))
