(ns chess-engine-clj.engine
    (:require [clojure.string :as string]))

(def initial-board [\r \n \b \q \k \b \n \r
                    \p \p \p \p \p \p \p \p
                    \- \- \- \- \- \- \- \-
                    \- \- \- \- \- \- \- \-
                    \- \- \- \- \- \- \- \-
                    \- \- \- \- \- \- \- \-
                    \P \P \P \P \P \P \P \P
                    \R \N \B \Q \K \B \N \R])

(def board-state (atom {:board initial-board
                        :turn "white"
                        :moves-cnt 0
                        :white-can-castle-ks true
                        :white-can-castle-qs true
                        :black-can-castle-ks true
                        :black-can-castle-qs true}))

(def uni-pieces {\R "♜", \N "♞", \B "♝", \Q "♛", \K "♚", \P "♟",
                 \r "♖", \n "♘", \b "♗", \q "♕", \k "♔", \p "♙", \- "·"})
;;---------

(defn line-convert [l-vec]
    (->> (map uni-pieces l-vec)
         (map #(str % " "))
         string/join))

(defn pretty-print []
    (->> (partition 8 initial-board)
         (map line-convert)
         (map #(str (- 8 %1) %2) (range 8))
         (string/join "\n")
         (#(str % "\n a b c d e f g h\n\n"))))

(println (pretty-print))

;;----------
