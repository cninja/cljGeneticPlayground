;Implementing http://www.ai-junkie.com/ga/intro/gat3.html
;Step 1: Get an arithmatic function that calculates to 42
(use 'clojure.math.numeric-tower)

(def enc {"0000" 0
          "0001" 1
          "0010" 2
          "0011" 3
          "0100" 4
          "0101" 5
          "0110" 6
          "0111" 7
          "1000" 8
          "1001" 9
          "1010" +
          "1011" -
          "1100" *
          "1101" /})
(defn to-gene-codes [chromo]
    (map (partial apply str) (partition 4 chromo)))
(defn from-gene-codes [genes]
  (apply str genes))
(defn decode [chromo]
  (map (partial enc)
       (map (partial apply str) (partition 4 chromo))))
(defn calc
  ([xs] (calc xs + 0))
  ([xs nextfn ans]
            (if (nil? xs)
              ans
              (let [f (first xs)
                    n (next xs)]
                (if (nil? nextfn)
                  (if (fn? f)
                    (recur n f ans)
                    (recur n nil ans))
                  (if (integer? f)
                    (if (and (= f 0) (= nextfn /)) ;Skip div by zero
                        (recur n nextfn ans)
                        (recur n nil (nextfn ans f)))
                    (recur n nextfn ans)))))) )


(defn score [chromo]
  (let [genes (decode chromo)
        c (calc genes)
        fitness (- 42 c)]
    (if (zero? fitness)
      (throw (Exception. chromo))
      (- 100
         (Math/min (Integer. 100) (Integer. (int (abs fitness))))))))

(defn build-population [pop-size gene-count genes]
  (let [rand-gene #(rand-nth genes)]
    (doall (repeatedly pop-size #(apply str (repeatedly gene-count rand-gene))))))
(defn maybe-sex [rate gene-options c1 c2]
  (if (= c1 c2)
    [c1 (maybe-mutate 1 gene-options c2)]
    (if (> rate (rand))
      (let [c1genes (to-gene-codes c1)
            c2genes (to-gene-codes c2)
            swap-at (rand-int (count c1genes))
            child1 (from-gene-codes (concat (take swap-at c1genes) (drop swap-at c2genes)))
            child2 (from-gene-codes (concat (take swap-at c2genes) (drop swap-at c1genes)))
            ]
        [child1 child2])
      [c1 c2])))
(defn maybe-mutate [rate gene-options chromo]
  (if (> rate (rand))
     (let [genes (to-gene-codes chromo)
           new-gene-idx (inc(rand-int (count genes)))]
       (from-gene-codes (concat (take (dec new-gene-idx) genes) (rand-nth gene-options) (drop new-gene-idx genes))) )
     chromo))
(defn select-member [population]
  (let [total-fitness (apply + (map score population))
        selected-parent-idx (rand)
        selector (fn [xs idx]
                   (let [f (first xs)
                         new-idx (+ idx (/ (score f) total-fitness))]
                     (if (> new-idx selected-parent-idx)
                       f
                       (recur (next xs) new-idx))))]
    (selector population 0)))
(defn gene-combine [crossover-rate mutate-rate gene-options chromo1 chromo2 ]
  (let [children (maybe-sex crossover-rate gene-options chromo1 chromo2)]
    (map (partial maybe-mutate mutate-rate gene-options) children)))
(defn next-generation [crossover-rate mutate-rate gene-options population]
  (flatten (repeatedly (/ (count population) 2)
                       #(gene-combine crossover-rate
                                     mutate-rate
                                     gene-options
                                     (select-member population)
                                     (select-member population)))))
(defn display [chromo]
  (let [genes (decode chromo)
        clarify #(condp = %
                   + "+"
                   - "-"
                   * "*"
                   / "/"
                   (.toString %))]
    (apply str (map clarify genes))))


;Actual Code
(def gene-count 20)
(def pop-size 40)
(def crossover-rate 0.7)
(def mutate-rate 0.001)
(try
  (loop [i 1
         population (build-population pop-size gene-count (keys enc))]
    (println (map #(calc (decode %))  population))
    (println "Average fitness: " (float (/ (apply + (map score population)) (count population))))
    (if (< i 100)
      (recur (inc i) (next-generation crossover-rate mutate-rate (keys enc) population))))
  (catch Exception e (println "FOUND IT: " (display (.getMessage e)))))

;TESTS
(map score (build-population 2 7 (keys enc)))
(select-member (build-population 2 7 (keys enc)))

(calc [6 + 5 * 4 / 2 + 1])
(calc [2 2 + nil - 7 2])
(score "011010100101110001001101001010100001")
(to-gene-codes "011010100101110001001101001010100001")
(maybe-sex 0.7 "00000000000000000000" "11111111111111111111" )
(maybe-mutate 0.7 (keys enc) "00000000000000000000")
(display  "0011101010001100010010110010")

(calc "1000000011000100100010101001" )
(map enc (to-gene-codes "0101110001110111001110010110 "))
(calc (decode "0101110001110111001110010110"))
(calc [5 * 7 7 3 9 6])
(calc (decode "0101110001110111001110010110 "))
(score "0101110001110111001110010110 ")
