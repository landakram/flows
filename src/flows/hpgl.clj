(ns flows.hpgl
  (:import [hpglgraphics HPGLGraphics])
  (:require [quil.applet :as ap]
            [quil.core :as q]
            [quil.util :as u]))

(def doc-width 11040)
(def doc-height 7721)

(defn create-hpgl-graphics [w h path]
  (.createGraphics (ap/current-applet) (int w) (int h)
                   HPGLGraphics/HPGL
                   (u/absolute-path path)))

(defn do-record [w h outfile f]
  (let [scale-val (if (> (/ w h) (/ doc-width doc-height))
                    (/ doc-width w)
                    (/ doc-height h))
        new-w (* w scale-val)
        new-h (* h scale-val)
        padding-w (/ (- doc-width new-w) 2)
        padding-h (/ (- doc-height new-h) 2)] 
    (println "scaling:" (float scale-val) "new-w:" (float new-w) "new-h:" (float new-h))
    (println "padding:" (float padding-w) (float padding-h))
    (q/do-record (create-hpgl-graphics doc-width doc-height outfile)
                 (q/translate padding-w padding-h)
                 (q/scale scale-val scale-val)
                 (f))))

