(ns flows.hpgl
  (:require [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [quil.applet :as ap]
            [quil.core :as q]
            [quil.util :as u]))

(def plotter-conf {:bin "/Users/mark/Documents/code/plot/plot.py"
                   :padding "1500"})

(defn plot [outfile]
  (let [outfile (u/absolute-path outfile)
        {:keys [bin padding]} plotter-conf]
    (println "Plotting...")
    (let [result (sh bin :env (merge {}
                                     (System/getenv)
                                     {"HPGL_FILE" outfile "PADDING" padding}))]
      (when (contains? result :err)
        (println "== ERROR ==")
        (println (:err result)))
      (println (:out result)))))
