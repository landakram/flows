(ns flows.core
  (:require [quil.core :refer :all]
            [flows.hpgl :as hpgl]
            [quil.helpers.seqs :refer [range-incl]]
            [quil.helpers.calc :refer [mul-add]]
            [clojure.pprint :refer [pprint]]
            [quil.middleware :as m]))

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

(defn initial-state []
  (let [width (width)
        height (height)
        tile-width 20
        tile-height 20
        origin [0 0]
        grid (make-grid tile-width tile-height width height origin)]
    {:grid grid
     :x-start (random 10)
     :y-start (random 10)}))

(defn setup []
  (frame-rate 60)
  (color-mode :hsb)
  (initial-state))

(defn update-state [state]
  state)

(defn draw-line [x y width height noise-factor]
  (push-matrix)
  (translate x y)
  (rotate (* noise-factor TWO-PI))
  (let [h (map-range noise-factor 0.0 1.0 0 360)
        s 300
        b 360 #_(map-range noise-factor 0.0 1.0 0 300)
        alpha (map-range noise-factor 0 1 100 255)
        length (+ width 20)
        start (- (/ length 2))
        end (/ length 2)]
    (stroke-weight 3)
    #_(stroke 0 0 0)
    (stroke h s b alpha)
    (line start 0 end 0)
    (pop-matrix)))

(defn draw-flows [{:keys [x-start y-start grid]}]
  (let [{:keys [tile-width tile-height]} grid
        grid-points (grid-points grid)]
    (doseq [[x y] grid-points]
      (let [x-noise (mul-add x 0.0030 x-start)
            y-noise (mul-add y 0.0030 y-start)
            noise-factor (noise
                          x-noise
                          y-noise
                          (*
                           (frame-count) 0.04))]
        (draw-line x y
                   tile-width
                   tile-height
                   noise-factor)))))

(defn draw-state [{:keys [x-start y-start grid] :as state}]
  #_(background 255)
  (background 31 31 20)
  
  (draw-flows state)
  #_(hpgl/do-record
   (width)
   (height)
   "generated/out.hpgl"
   (fn []
     (println "Recording to hpgl...")
     (draw-flows state)
     (println "Done.")
     (no-loop))))

(defsketch flows
  :title "Flows"
  :settings
  (fn []
    (smooth 8))
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
