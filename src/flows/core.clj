(ns flows.core
  (:require [quil.core :refer :all]
            #_[plotter-utils.grid :refer :all]
            [plotter-utils.quil :as pl]
            [data.grid2d :as grid]
            [quil.helpers.calc :refer [mul-add]]
            [clojure.pprint :refer [pprint]]
            [quil.middleware :as m]))

(defn transform-fn [x-start y-start]
  (fn [[x y] val]
    (let [x-noise (mul-add x 0.030 x-start)
          y-noise (mul-add y 0.030 y-start)
          z-noise (* (frame-count) 0.030)]
      (assoc val :noise (noise x-noise y-noise z-noise)))))

(defn make-grid [origin width height tile-width tile-height x-start y-start]
  (let [columns (/ width tile-width)
        rows (/ height tile-height)
        transform (transform-fn x-start y-start)]
    {:g (grid/create-grid columns rows
                          (fn [[x y]]
                            (transform [x y] {})))
     :width width
     :height height
     :tile-width tile-width
     :tile-height tile-height
     :origin origin}))

(defn make-curve [origin step-length steps]
  {:origin origin
   :step-length step-length
   :steps steps})

(defn distort-angle [[x y] angle]
  (let [rect {:x1 200 :y1 200 :x2 200 :y2 650}
        {:keys [x1 y1 x2 y2]} rect]
    (cond
      (and (= 200)
           (= 200))
      (and (> x x1)
           (< x x2)
           (> y y1)
           (< y y2))
      (* TWO-PI -0.12)
      :else
      angle)))

(defn get-angle [[x y] {:keys [g tile-width tile-height] :as grid}]
  (let [col (int (/ x tile-width))
        row (int (/ y tile-height))
        cell (get g [col row])]
    (when-not (nil? cell)
      (->> (* (:noise cell) TWO-PI)
           (distort-angle [x y])))))

(defn enumerate-segments [{:keys [origin step-length steps]}
                          {:keys [g tile-width tile-height] :as grid}]
  (let [[x y] origin]
    (loop [last-x x
           last-y y
           segments []]
      (let [angle (or (get-angle [last-x last-y] grid)
                      -1)
            x-step (* step-length (cos angle))
            y-step (* step-length (sin angle))
            x (+ last-x x-step)
            y (+ last-y y-step)]
        (if (or (= angle -1)
                (>= (count segments) steps))
          segments
          (recur
           x
           y
           (conj segments [x y])))))))

(defn initial-state []
  (let [w (* (width) 1.5)
        h (* (height) 1.5)
        tile-width 10
        tile-height 10
        origin [(- (/ (- w (width)) 2))
                (- (/ (- h (height)) 2))]
        x-start (random 10)
        y-start (random 10)
        grid (make-grid origin w h tile-width tile-height x-start y-start)]
    {:grid grid
     :curves (take 1000 (repeatedly #(make-curve [(rand-int w) (rand-int h)] 10 30)))
     :x-start x-start
     :y-start y-start}))

(defn enumerate-cells [{:keys [g tile-height tile-width]}]
  (for [[x y] (grid/posis g)]
    [[(* x tile-width) (* y tile-height)] (get g [x y])]))

(defn setup []
  (frame-rate 60)
  (color-mode :hsb)
  (initial-state))


(defn update-grid [{:keys [g] :as grid} x-start y-start]
  (-> grid
      (update :g #(grid/transform % (transform-fn x-start y-start)))))

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

(defn draw-curve [curve grid]
  (let [{:keys [origin]} curve
        [x y] origin
        {:keys [tile-height tile-width]} grid
        scaled-x (int (/ x tile-width))
        scaled-y (int (/ y tile-height))
        el (get (:g grid) [scaled-x scaled-y])
        noise (:noise el)
        h (map-range noise 0.0 1.0 0 360)]
    #_(push-style)
    #_(stroke 0)
    #_(stroke h 300 360)
    (begin-shape)
    (doseq [[x y] (enumerate-segments curve grid)]
      (vertex x y))
    (end-shape)
    #_(pop-style)))

(defn draw-flows [{:keys [x-start y-start grid curves]}]
  (let [{:keys [tile-width tile-height]} grid]
    (doseq [curve curves]
      (draw-curve curve grid))
    #_(doseq [[[x y] {:keys [noise]}] (enumerate-cells grid)]
      (draw-line x y
                 tile-width
                 tile-height
                 noise))))

(defn draw-state [{:keys [x-start y-start grid] :as state}]
  (background 255)
  #_(background 31 31 20)
  
  (no-fill)

  (draw-flows state)
  (no-loop)
  #_(save-frame "generated/curve-####.png")
  (do
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
  :size [800 1000]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
