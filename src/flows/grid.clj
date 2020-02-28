(ns flows.grid
  (:require  
   [quil.helpers.seqs :refer [range-incl]]))

(defn grid-points [{:keys [origin width height tile-width tile-height]}]
  (let [[origin-x origin-y] origin]
    (for [y (range-incl origin-x (+ origin-x height) tile-height)
          x (range-incl origin-y (+ origin-y width) tile-width)]
        [x y])))

(defn make-grid [tile-width tile-height width height origin]
  {:origin origin
   :width width
   :height height
   :tile-width tile-width
   :tile-height tile-height})
