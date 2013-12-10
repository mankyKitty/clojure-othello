(ns othello.core
  (:use [clojure.string :only [join split upper-case lower-case]]
        [clojure.core.match :only (match)])
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

(defn switch-player [p]
  ;; Give the opposite player to the one given.
  (if (= p :black)
    :white
    :black))

(defn printable-name [p]
  ;; Give a screen friendly version of the current player.
  (match p
   :black "Black"
   :white "White"))

(defn calc-move [sq max func]
  ;; Given a starting place, board edge size and
  ;; direction - return the next square in that
  ;; direction.
  (let [new-sq ((func moves) sq max)]
    (when (and (> new-sq 0) (< new-sq (* max max)))
      {:dir func :sqr new-sq})))

(defn get-moves [sq max]
  ;; From a given starting place, find the valid directions.
  ;; (filter #(not (nil? %)) (map #(calc-move sq max %) (keys moves)))
  (filter identity (map (partial calc-move sq max) (keys moves))))

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
    (let [col (first (.getBytes (upper-case c)))]
      (when (and (>= col upper-case-char-int)
                 (<= col (+ upper-case-char-int max)))
        ;; Pass the tests and it's okay.
        col))))

(defn parse-input [input row-col max]
  (if (= row-col :row)
    (parse-row-input input max)
    (parse-col-input input max)))

(defn convert-move-input [row col max]
  ;; take a row and column and convert it
  ;; to a piece placement location.
  (let [r (parse-input row :row max)
        c (parse-input col :col max)]
    (+ (* max (- r 1)) (- c upper-case-char-int))))

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

(defn opp-player-sqr? [player sqr board]
  ;; Check if a square belongs to the other player
  (not= player (:status (nth board sqr))))

(defn filter-tiles-by-not-status [pred coll board]
  ;; Given a square status, a list of squares and a current play
  ;; board, filter the list of squares to ones that match the
  ;; predicate or nil if there is nothing.
  (not-empty (filter #(not= pred (:status (nth board (:sqr %)))) coll)))

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

(defn move-sequence [start direction max]
  ;; Return a lazy sequence of every tile in a given direction that
  ;; ends when there are no more valid moves in that direction. Does
  ;; not take into account the status of the tiles. Don't be greedy :(
  (lazy-seq (take-while identity (iterate #(:sqr (calc-move % max direction)) start))))

(defn valid-move-directions [player row col-char max board]
  ;; Take player input and get a list of the valid directions that the
  ;; player is allowed to move.
  (let [target (convert-move-input row col-char max)
        all-moves (get-moves target max)]
    (any-opp-colour-adjacent player (any-adjacent-tiles all-moves board) board)))

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

(defn create-board-adjustment [moves player]
  (for [{:keys [status sqr] :as original} moves
        :while (and (not= :empty status) (not= player status))]
    (hash-map original {:status (switch-player status) :sqr sqr})))

(defn determine-valid-flips [moves player]
  (let [partitioned (partition-by #(= (switch-player player) (:status %)) moves)]
    (loop [[flips cap & xs] partitioned
           acc ()]
      (if (or (not (seq flips)) ;; We have nothing new to add to the acc
              (not (seq cap)) ;; Our list cannot be ended with the players colour
              (not= player (:status (first cap))) ;; The first part of the cap is not the current player
              (= :empty (:status (first cap)))) ;; Our list is out of pieces
        acc ;; Give back the list of tiles that are to be flipped
        (recur xs (apply conj acc flips))))))

(defn get-flippable-tiles [board max player start]
  ;; From a given starting square, create a list of the tiles that are fippable.
  (let [moves (->> (move-sequence (:sqr start) (:dir start) max)
                   (sort-by :sqr)
                   (map #(nth board %)))]
    (when-let [valid-moves (determine-valid-flips moves player)]
      (create-board-adjustment valid-moves player))))

(defn squares-to-flip [starting-places board max player]
  ;; Get a list of the squares that have tiles we want to flip.
  (let [[fst & others] (->> starting-places
                            (map #(get-flippable-tiles board max player %))
                            (filter identity)
                            (flatten))]
    (if (seq others)
      (apply merge fst others)
      fst)))

(defn parse-int [s]
  (try (Integer/parseInt s)
    (catch NumberFormatException e nil)))

(defn process-input [s]
  ;; Take the read-line input and get something we can try to use as
  ;; valid move input.
  (let [[row col] (filter #(not= "" %) (split s #""))]
    [(parse-int row) col]))

(defn get-player-score [brd]
  (let [[b e w] (->> brd
                     (sort-by :status)
                     (partition-by :status))]
    (hash-map :b (count b)
              :w (count w))))

(defn update-display [player current-board max]
  (let [{:keys [b w]} (get-player-score current-board)]
    (do
      (println "Scores !")
      (println (str "Black : " b))
      (println (str "White : " w))
      ;; Display the latest board.
      (print-board current-board max)
      ;; Ask the current player for their desired move.
      (println (str (printable-name player) " Player's Turn!"))
      (println "Enter Row and Column Number (eg. 3d) or Q/q to quit:"))))

(defn get-placement-and-directions [row col brd max player]
  (when (and (not (nil? row))
             (not (nil? col))
             (number? row))
    (let [plc (convert-move-input row col max)
          dirs (valid-move-directions player row col max brd)]
      (when (and
              (= :empty (:status (nth brd plc)))
              (seq dirs))
        [plc dirs]))))

(defn quitting? [row col]
  (cond
    (nil? row) (= "q" (lower-case col))
    (nil? col) (= "q" (lower-case row))
    :else false))

(defn game-loop [max board]
  (loop [game-board board
         player :black
         turn 0]
    (update-display player game-board max)
    ;; Try to do something with it all.
    ;; If we have a parse-able input that isn't a quit instruction
    ;; and that move produces a valid change on the board THEN we want to
    ;; actually progress the game forward.
    (let [player-input (read-line)
          [row col] (process-input player-input)]

      (if-let [[placement dirs] (get-initial-placement row col game-board max player)]

        (let [flip (squares-to-flip dirs game-board max player)
              all-moves (merge flip {{:status :empty :sqr placement}
                                     {:status player :sqr placement}})]
          ;; Assuming we have a valid placement, try to make the move(s).
          ;; Before looping the board I have to replace any changing squares that
          ;; have resulted from any new moves. The result of a move is
          (recur (replace all-moves game-board) (switch-player player) (inc turn)))

        (if (= "q" player-input)
          (println "Quitting...")
          (recur game-board player turn))))))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (let [max 8
        board (make-board max)]
    (game-loop max board)))
