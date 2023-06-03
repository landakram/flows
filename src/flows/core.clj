(ns flows.core
  (:require [quil.core :refer :all]
            #_[plotter-utils.grid :refer :all]
            [plotter-utils.quil :as pl]
            [data.grid2d :as grid]
            [quil.helpers.calc :refer [mul-add]]
            [clojure.pprint :refer [pprint]]
            [quil.middleware :as m]))

(def println pprint)

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

(defn circle-point [theta {:keys [r x y]}]
  [(+ x (* r (cos theta)))
   (+ y (* r (sin theta)))])

(defn tan-line-slope [theta circle]
  (let [next-point (circle-point (+ theta 0.1) circle)
        prev-point (circle-point (- theta 0.1) circle)
        dx (- (first next-point) (first prev-point))
        dy (- (last next-point) (last prev-point))]
    (when-not (= (int dx) 0)
      (/ dy dx))))

(defn distance-from-perim [[x y] circle]
  (let [dx (- x (:x circle))
        dy (- y (:y circle))]
    (- (pow (:r circle) 2)
       (+ (pow dx 2)
          (pow dy 2)))))

(defn in-circle? [[x y] circle]
  (>= (distance-from-perim [x y] circle)
      0))

(defn theta-for-line-tangent-to-circ [dx dy circle]
  (if (= (int dx) 0)
    0
    (let [slope (/ dy dx)
          theta (atan slope)
          tan-line-slope (tan-line-slope theta circle)]
      (if (nil? tan-line-slope)
        (/ PI 2)
        (let [tan-line-theta (atan tan-line-slope)]
          tan-line-theta)))))

(defn in-some-circle [circles [x y]]
  (first (filter #(in-circle? [x y] %) circles)))

(defn distort-angle [[x y] distortions angle]
  (let [circle (in-some-circle distortions [x y])]
    (cond
      (some? circle)
      (let [r (:r circle)
            dx (- x (:x circle))
            dy (- y (:y circle))
            distance-from-perim (abs (distance-from-perim [x y] circle))
            theta (mod (theta-for-line-tangent-to-circ dx dy circle) TWO-PI)
            other-direction-theta (mod (- theta PI) TWO-PI)
            new-angle (if (> (abs (- angle theta)) (abs (- angle other-direction-theta)))
                        other-direction-theta
                        theta)
            percent-dist-from-perim (/ distance-from-perim (pow r 2))
            max-dist 0.5
            new-angle-weight (min 1 (/ percent-dist-from-perim max-dist))]
        (+ (* new-angle new-angle-weight)
           (* angle (- 1 new-angle-weight))))
      :else
      angle)))

(defn get-angle [[x y] distortions {:keys [g tile-width tile-height] :as grid}]
  (let [col (int (/ x tile-width))
        row (int (/ y tile-height))
        cell (get g [col row])]
    (when-not (nil? cell)
      (->> (* (:noise cell) TWO-PI)
           (distort-angle [x y] distortions)))))

(defn enumerate-segments [{:keys [origin step-length steps]}
                          distortions
                          {:keys [g tile-width tile-height] :as grid}]
  (let [[x y] origin]
    (loop [last-x x
           last-y y
           segments []]
      (let [angle (or (get-angle [last-x last-y] distortions grid)
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

(defn random-points [w h]
  (repeatedly (fn [] [(rand-int w) (rand-int h)])))

(defn random-curve [w h step-length steps distortions]
  (let [origin (first (drop-while #(some? (in-some-circle distortions %)) (random-points w h)))]
    (make-curve origin step-length steps)))

(defn random-distortions [min-x max-x min-y max-y min-r max-r]
  (repeatedly (fn [] {:x (random min-x max-x)
                      :y (random min-y max-y)
                      :r (random min-r max-r)})))

(defn initial-state []
  (let [w (* (width) 1.5)
        h (* (height) 1.5)
        tile-width 10
        tile-height 10
        origin [(- (/ (- w (width)) 2))
                (- (/ (- h (height)) 2))]
        x-start (random 10)
        y-start (random 10)
        distortions (take (rand-nth (range 1 10))
                          (random-distortions 0 (width) 0 (height) 50 150))
        grid (make-grid origin w h tile-width tile-height x-start y-start)]
    {:grid grid
     :distortions distortions
     :curves (take 2500 (repeatedly #(random-curve w h 10 30 distortions)))
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

(defn draw-curve [curve distortions grid]
  (let [{:keys [origin]} curve
        [x y] origin
        {:keys [tile-height tile-width]} grid
        scaled-x (int (/ x tile-width))
        scaled-y (int (/ y tile-height))
        el (get (:g grid) [scaled-x scaled-y])
        noise (:noise el)
        h (map-range noise 0.0 1.0 0 360)]
    #_(push-style)
    #_(stroke 255)
    #_(stroke 0 0 0)
    (let [point-pairs (partition 2 1 (enumerate-segments curve distortions grid))]
      (doseq [[p1 p2] point-pairs]
        (line (p1 0) (p1 1) (p2 0) (p2 1))))
    #_(pop-style)))



(defn draw-flows [{:keys [x-start y-start distortions grid curves]}]
  (let [{:keys [tile-width tile-height]} grid]
    (doseq [curve curves]
      (draw-curve curve distortions grid))))

(defn draw-state [{:keys [x-start y-start distortions grid] :as state}]
  (background 255)
  #_(background 31 31 20)
  
  (no-fill)

  #_(doseq [{:keys [x y r]} distortions]
    (ellipse x y (* r 2) (* r 2)))
  
  (draw-flows state)
  (save "generated/curve-distortions-3.png")
  (no-loop)
  #_(save-frame "generated/curve-####.png")
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
  :size [800 1000]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
