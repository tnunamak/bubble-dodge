(ns tinygame.core
  (:require [lanterna.screen :as s]))

(use 'overtone.at-at)
(use '[clojure.math.numeric-tower :as math])

(defn clear-screen [scr]
              (let [[cols rows] (vec (s/get-size scr))]
                (doseq [x (range cols)
                        y (range rows)]
                  (s/put-string scr x y " "))))

(defn draw-pt [scr x y]
  (s/put-string scr x y "*"))

(defn draw-circle [scr x y radius]
  (let [[cols rows] (s/get-size scr)]
    (doseq [xi (range cols)
            yi (range rows)]

      (if (< (+ (math/expt (- xi x) 2)
                (math/expt (- yi y) 2))
             (math/expt radius 2))
            (draw-pt scr xi yi)))))

(defn get-rand-point [scr]
  (conj (vec (map #(Math/round (rand %)) (s/get-size scr))) 1))

(defn draw-points [scr game]
  (doseq [[x y radius] (:points game)]
    (draw-circle scr x y radius)))

(defn collapse-big-points [game]
  (let [x (filter
             #(let [[x y rad] %] (< rad 10))
             (:points game))]
    {:points x}))

(defn grow [game]
  {:points (map #(let [[x y rad] %]
                      [x y (+ 1 rad)]) (:points game))})

(defn recalculate [game scr]
  (let [new-game (collapse-big-points game)]
    (if (> 0.3 (rand))
      {:points (conj (:points (grow new-game)) (get-rand-point scr))}
      (grow new-game))))

(defn go []

    (def points (atom #{}))
    (def scr (s/get-screen))

    (s/start scr)

    (def my-pool (mk-pool))

    (defn draw [game]
      (let [new-game (recalculate game scr)]
        (do
          (clear-screen scr)

          (draw-points scr new-game)

          (s/redraw scr)
          (at (+ 500 (now)) #(draw new-game) my-pool))))

    (draw {:points #{}}))

;(let [inp (s/get-key-blocking scr)]
;              (if (= (str inp) "x") (s/stop scr)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (go))
