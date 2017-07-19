(ns chess-engine-clj.core
    (:require [chess-engine-clj.engine :as engine])
  (:gen-class))

(defn -main
  []
  (while true
  	(engine/game-play)))
