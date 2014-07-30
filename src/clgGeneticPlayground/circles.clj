;Implementing http://www.ai-junkie.com/ga/intro/gat3.html
;Step2: Find the largest circle that will fit within a set of non-overlapping circles
(use 'clojure.math.numeric-tower)
(use 'seesaw.core)
(use 'seesaw.graphics)
(use 'seesaw.color)


(def root-atoms (atom []))
(defn add-root! [r] (reset! root-atoms (conj @root-atoms r)))
(defn by-id [id] (first (remove nil? (map #(select % [id]) @root-atoms))))

(defn not-circle-intersect [[x1 y1 r1] [x2 y2 r2]]
  (let [x-diff (- x1 x2)
        y-diff (- y1 y2)]
    (> (Math/sqrt (+ (* x-diff x-diff) (* y-diff y-diff))) (+ r1 r2))))

(def enc {"000" 0
          "001" 1
          "010" 2
          "011" 3
          "100" 4
          "101" 5
          "110" 6
          "111" 7
          })
(defn to-gene-codes [chromo]
    (map (partial apply str) (partition 3 chromo)))
(defn from-gene-codes [genes]
  (apply str genes))

;The Circle chromosome is defined by three numbers encoded in octal
;width height radius are all composed of three octal digits
;which are devided by 0777 to determine the fraction of total board area
;In other words a circle of the form 101000000011000000001000000 decodes
;  to 0500 0300 0100 which represents a circle with its center at 5/8th
;  of the way from the x axis and 3/8th of the way from the y axis with a
;  radius of 1/8ths of the board's width
(defn decode [chromo board-width board-height]
  (let [numbers (map (partial enc) (to-gene-codes chromo))
       [x-frac y-frac r-frac] (map #(/ (Integer/parseInt (apply str %) 8) 01000) (partition 3 numbers))]
    [(int (* x-frac board-width)) (int (* y-frac board-height)) (int (* r-frac board-width)) false]))
;(display [(decode "101000000011000000001000000" board-width board-height)])

;sutability testing
;0 for a circle that falls off the edge or overlaps one of the base circles
;calculated as the area of the circle otherwise
(defn score [chromo base-circles board-width board-height]
  (let [[x y r :as circle] (decode chromo board-width board-height)
        ]
    (if (and (every? (partial not-circle-intersect circle) base-circles)
             (< r x)
             (< r y)
             (< (+ x r) board-width)
             (< (+ y r) board-height))
      r
      0)))

(defn build-population [pop-size gene-count genes]
  (let [rand-gene #(rand-nth genes)]
    (doall (repeatedly pop-size #(apply str (repeatedly gene-count rand-gene))))))
(defn maybe-mutate [rate gene-options chromo]
  (if (> rate (rand))
     (let [genes (to-gene-codes chromo)
           new-gene-idx (inc(rand-int (count genes)))]
       (from-gene-codes (concat (take (dec new-gene-idx) genes) (rand-nth gene-options) (drop new-gene-idx genes))) )
     chromo))
(defn maybe-sex [rate gene-options c1 c2 score-fn]
  (if (= c1 c2)
    [c1 (maybe-mutate 1 gene-options c2)]
    (if (> rate (rand))
      (let [c1genes (to-gene-codes c1)
            c2genes (to-gene-codes c2)
            swap-at (rand-int (count c1genes))
            child1 (from-gene-codes (concat (take swap-at c1genes) (drop swap-at c2genes)))
            child2 (from-gene-codes (concat (take swap-at c2genes) (drop swap-at c1genes)))
            ]
        (if (and (> (score-fn child1) 0)
                 (> (score-fn child2) 0))
          [child1 child2]
          (recur rate gene-options c1 c2 score-fn)))
      [c1 c2])))
(defn select-member [population score-fn]
  (let [total-fitness (apply + (map score-fn population))
        selected-parent-idx (rand)
        selector (fn [xs idx]
                   (let [f (first xs)
                         new-idx (+ idx (/ (score-fn f) total-fitness))]
                     (if (> new-idx selected-parent-idx)
                       f
                       (recur (next xs) new-idx))))]
    (selector population 0)))
(defn gene-combine [crossover-rate mutate-rate gene-options chromo1 chromo2 score-fn ]
  (let [children (maybe-sex crossover-rate gene-options chromo1 chromo2 score-fn)]
    (map (partial maybe-mutate mutate-rate gene-options) children)))
(defn next-generation [crossover-rate mutate-rate gene-options population score-fn]
  (flatten (repeatedly (/ (count population) 2)
                       #(gene-combine crossover-rate
                                     mutate-rate
                                     gene-options
                                     (select-member population score-fn)
                                     (select-member population score-fn)
                                      score-fn))))

(defn circle-draw-commands [[x y r is-base]]
  (let [bg-color (if is-base "#ffffff" "#ff0000")]
    [(circle x y r) (style :foreground "#000000" :background bg-color)]))
(defn display [circles]
  (let [items-to-display (apply concat (map circle-draw-commands circles))]
    (config! (by-id :#canvas)  :paint   (fn [c g] (apply draw g items-to-display)))))

(defn generate-board [circle-count w h]
  (repeatedly circle-count
              #(let [r (inc (rand-int (/ (Math/min w h) 6)))]
                 (vector (rand-int (- w r)) (rand-int (- h r)) r true))))



;(display )
;(defn score [chromo base-circles board-width board-height]

;Actual Code
(def gene-count 9) ;Cannot be changed for circles (see score fn)
(def pop-size 400)
(def crossover-rate 0.7)
(def mutate-rate 0.001)
(def board-height 700)
(def board-width 800)
(def f (frame :id :main-frame :title "Fun with Genetics" :width board-width :height board-height))
(let [base-circles (generate-board 15 board-width board-height)
      score-fn #(score % base-circles board-width board-height)]
  (reset! root-atoms [(-> f show!)])
  (native!)
  (config! f :content (border-panel :center (canvas :id :canvas :background "#ffffff" :paint nil)))
  (display base-circles)
  (loop [i 1
         population (build-population pop-size gene-count (keys enc))]
    (println (map score-fn population))
    (display (concat base-circles (map #(decode % board-width board-height) population)))
    ;(println "Average fitness: " (float (/ (apply + (map score population)) (count population))))
    (if (< i 100)
      (recur (inc i) (next-generation crossover-rate mutate-rate (keys enc) population score-fn))
     ; (display (concat base-circles (map #(decode % board-width board-height) population)))
      )))
