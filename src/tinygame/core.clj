(ns tinygame.core
  (:require [lanterna.screen :as s]))

(use 'overtone.at-at)
(use '[clojure.math.numeric-tower :as math])

(defn clear-screen [scr]
              (let [[cols rows] (vec (s/get-size scr))]
                (doseq [x (range cols)
                        y (range rows)]
                  (s/put-string scr x y " "))))

(defn process-input [scr game]
  (let [[x y] (:cursor game)]
    (let [new-cursor (case (s/get-key scr)
                       :left [(- x 1) y]
                       :right [(+ x 1) y]
                       :up [x (- y 1)]
                       :down [x (+ y 1)]
                       [x y])]
      (s/move-cursor scr (get new-cursor 0) (get new-cursor 1))
      (assoc-in game [:cursor] new-cursor))))

(defn in-circle [x y radius xi yi]
  (< (+ (math/expt (- xi x) 2)
                (math/expt (- yi y) 2))
             (math/expt radius 2)))

(defn draw-score [scr game]
  (s/put-string scr 0 0 (str (:score game))))

(defn score [scr game]
  (if (seq (filter #(apply in-circle (concat % (:cursor game))) (:points game)))
    (assoc-in game [:status] :game-over)
    (assoc-in game [:score] (+ 1 (:score game)))))

(defn draw-pt [scr x y]
  (s/put-string scr x y "*"))

(defn draw-circle [scr x y radius]
  (let [[cols rows] (s/get-size scr)]
    (doseq [xi (range cols)
            yi (range rows)]
      (if (in-circle x y radius xi yi)
            (draw-pt scr xi yi)))))

(defn get-rand-point [scr]
  (conj (vec (map #(Math/round (rand %)) (s/get-size scr))) 1))

(defn draw-points [scr game]
  (doseq [[x y radius] (:points game)]
    (draw-circle scr x y radius)))

(defn collapse-big-points [game]
  (assoc-in game [:points] (filter
                               #(let [[x y rad] %] (< rad 10))
                               (:points game))))

(defn grow [game]
  (assoc-in game [:points] (map #(let [[x y rad] %]
                        [x y (+ 1 rad)]) (:points game))))

(defn recalculate [game scr]
  (let [new-game (collapse-big-points game)]
    (if (> 0.8 (rand))
      (assoc-in game [:points] (conj (:points (grow new-game)) (get-rand-point scr)))
      (grow new-game))))

(defn go []
    (def scr (s/get-screen))

    (s/start scr)

    (def my-pool (mk-pool))

    (defn tick [game tick-count]
      (let [new-game (if (= (mod tick-count 25) 0)
            (process-input scr (recalculate game scr))
            (process-input scr game))]
        (do
          (clear-screen scr)

          (draw-points scr new-game)
          (draw-score scr new-game)

          (s/redraw scr)
          (let [scored-game (score scr new-game)]
            (if (not= (:status new-game) :game-over)
                (at (+ 10 (now)) #(tick scored-game (inc tick-count)) my-pool))))))

    (tick {:points #{} :cursor (get-rand-point scr) :score 0} 0))

;(let [inp (s/get-key-blocking scr)]
;              (if (= (str inp) "x") (s/stop scr)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (go))
