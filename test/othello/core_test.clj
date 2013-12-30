(ns othello.core-test
  (:require [clojure.test :refer :all]
            [othello.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(def problem-moves
  "This is a set of moves that expose a weakness in the move
determination algorithms."
  [3 \d
   5 \c
   6 \d
   3 \e
   3 \f
   2 \e
   5 \f])

(defn sp2 [col]
  "split-at doesn't seem to appreciate being stuck in a /partial/ so
this will do."
  (split-at 2 col))

(defn run-sequence [moves max]
  "Take a given sequence of moves for a board of a given size and
return the final board state after the completion of all of the
provided moves."  
  (let [board (make-board max)
        first-move (first (sp2 moves))
        rest-of (second (sp2 moves))]
    
    (loop [mv first-move
           rest rest-of
           brd board
           player :black]
      
      (if (seq mv)

        (let [[row col] mv
              [placement dirs] (get-placement-and-directions row col brd max player)]
          (recur (first (sp2 rest))
                 (second (sp2 rest))
                 (update-game-board (move-and-flips player placement dirs brd max) brd)
                 (switch-player player)))
        
        brd))))
