(ns othello.core
  (:use [clojure.string :only [join]])
  (:gen-class))

(def moves {:up         (fn [s max] (- s max))
            :down       (fn [s max] (+ s max))
            :left       (fn [s max] (- s 1))
            :right      (fn [s max] (+ s 1))
            :up-left    (fn [s max] (- s (- max 1)))
            :up-right   (fn [s max] (- s (+ max 1)))
            :down-left  (fn [s max] (+ s (- max 1)))
            :down-right (fn [s max] (+ s (+ max 1)))})

(def square-states {:empty " "
                    :black "B"
                    :white "W"})

(def upper-case-char-int 65)

(defn centre-top-left [n]
  ;; Used to find the inner square for
  ;; placing the starting pieces.
  (* (- (/ n 2) 1) (+ n 1)))

(defn starting-places [n]
  ;; Get the locations of our starting pieces
  (let [start (centre-top-left n)]
    (cons start
          (map
           #(:sqr (calc-move start n %))
           ;; We only need a few points to fill
           [:right :down :down-right]))))

(defn valid-board-size? [n]
  ;; Check if we've been given a valid
  ;; board size. Must be even!
  (= (mod n 2) 0))

(defn parse-row-input [n max]
  (when (and (integer? n)
             (> n 0)
             (< n max))
    ;; Pass the tests and it's okay.
    n))

(defn parse-col-input [c max]
  (when (or (char? c) (string? c))
    (let [col (first (.getBytes (clojure.string/upper-case c)))]
      (when (and (>= col upper-case-char-int)
                 (<= col (+ upper-case-char-int max)))
        ;; Pass the tests and it's okay.
        col))))

(defn parse-input [input row-col max]
  (if (= row-col :row)
    (parse-row-input input max)
    (parse-col-input input max)))

(defn declare-move [row col max]
  ;; take a row and column and convert it
  ;; to a piece placement location.
  (+ (* max (- row 1)) (- (int col) upper-case-char-int)))

(defn create-square [start-locations location]
  ;; The order of the given start locations is important
  ;; We use them to determine the colour of the starting
  ;; square. Less fighting with hash maps.
  (if (not-any? #(= location %) start-locations)
    {:status :empty :sqr location}
    {:status (if (or
                  (= (nth start-locations 0) location)
                  (= (nth start-locations 3) location))
               :white
               :black)
     :sqr location}))

(defn square-empty? [sqr]
  ;; Is the square empty?
  (= :empty (:status sqr)))

(defn filter-tiles-by-not-status [pred coll board]
  ;; Given a square status, a list of squares and a current play
  ;; board, filter the list of squares to ones that match the
  ;; predicate or nil if there is nothing.
  (not-empty (filter #(not (= pred (:status (nth board (:sqr %))))) coll)))

(defn any-adjacent-tiles [moves board]
  ;; Using our status filter, find if there are any adjacent tiles
  ;; that actually have a tile in them. So we can check for moves that
  ;; would place a piece in the middle of no where.
  (filter-tiles-by-not-status :empty moves board))

(defn any-opp-colour-adjacent [player adjacent board]
  ;; Check if the given list of squares that we expect are adjacent to
  ;; our given tile (but don't have to be) are of the opposite colour.
  (filter-tiles-by-not-status player adjacent board))

(defn make-board [n]
  ;; Construct the initial board state including
  ;; the placement of the initial four pieces.
  (let [locations (starting-places n)]
    (map #(create-square locations %) (range 0 (* n n)))))

(defn print-col-headings [n]
  ;; Nicely spaced letters!
  (print
   (apply str
          "    "
          (join "   " (map char (range upper-case-char-int (+ upper-case-char-int n))))
          "\n")))

(defn get-row-id [location max]
  ;; Take the first location on a row and convert it
  ;; to a row id we can use to print stuff.
  (let [row-id (inc (/ location max))]
    (if (< row-id 10)
      (str " " row-id)
      row-id)))

(defn print-board [b n]
  ;; Print the given board to the screen.
  (print-col-headings n)
  (doseq [row (partition n b)]
    (print (str (get-row-id (:sqr (first row)) n) "| "))
    (doseq [sqr row]
      (print (str ((:status sqr) square-states) " | ")))
    (print "\n")))

(defn calc-move [sq max func]
  ;; Given a starting place, board edge size and
  ;; direction - return the next square in that
  ;; direction.
  (let [new-sq ((func moves) sq max)]
    (when (and (> new-sq 0) (< new-sq (* max max))) 
      {:dir func :sqr new-sq})))

(defn get-moves [sq max]
  ;; From a given starting place, find the valid directions.
  (filter #(not (nil? %)) (map #(calc-move sq max %) (keys moves))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
