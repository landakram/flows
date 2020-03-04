(ns flows.core
  (:require [quil.core :refer :all]
            #_[plotter-utils.grid :refer :all]
            [plotter-utils.quil :as pl]
            [data.grid2d :as grid]
            [quil.helpers.calc :refer [mul-add]]
            [clojure.pprint :refer [pprint]]
            [quil.middleware :as m]))

(defn make-grid [origin width height tile-width tile-height]
  (let [columns (/ width tile-width)
        rows (/ height tile-height)]
    {:g (grid/create-grid columns rows
                     (fn [[x y]]
                       {:noise (noise x y 0)}))
     :width width
     :height height
     :tile-width tile-width
     :tile-height tile-height
     :origin origin}))

(defn initial-state []
  (let [width (width)
        height (height)
        tile-width 20
        tile-height 20
        origin [0 0]
        grid (make-grid origin width height tile-width tile-height)]
    {:grid grid
     :x-start (random 10)
     :y-start (random 10)}))

(defn enumerate-cells [{:keys [g tile-height tile-width]}]
  (for [[x y] (grid/posis g)]
    [[(* x tile-width) (* y tile-height)] (get g [x y])]))

(defn setup []
  (frame-rate 60)
  (color-mode :hsb)
  (initial-state))

(defn update-grid [{:keys [g] :as grid} x-start y-start]
  (let [transform-fn (fn [[x y] val]
                       (let [x-noise (mul-add x 0.030 x-start)
                             y-noise (mul-add y 0.030 y-start)
                             z-noise (* (frame-count) 0.030)]
                         (assoc val :noise (noise x-noise y-noise z-noise))))]
    (-> grid
        (update :g #(grid/transform % transform-fn)))))

(defn update-state [{:keys [x-start y-start] :as state}]
  (-> state
      (update :grid #(update-grid % x-start y-start))))

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
  (let [{:keys [tile-width tile-height]} grid]
    (doseq [[[x y] {:keys [noise]}] (enumerate-cells grid)]
      (draw-line x y
                 tile-width
                 tile-height
                 noise))))

(defn draw-state [{:keys [x-start y-start grid] :as state}]
  #_(background 255)
  (background 31 31 20)
  
  (draw-flows state)
  #_(do
    (let [out "generated/out.hpgl"]
      (pl/do-record
       (width)
       (height)
       out
       (fn []
         (println "Recording to hpgl...")
         (draw-flows state)
         (println "Done.")
         (no-loop))))))

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
