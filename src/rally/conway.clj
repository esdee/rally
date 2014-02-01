(ns rally.conway
  (:require [clojure.string :as str]))


(defn- cell
  "Given an x and y coord return the cell. Nil if it does not exist."
  [board x y]
  (first
    (filter (fn [[cx cy _]]
              (and (= x cx) (= y cy)))
            board)))

;TODO use a for here instead
(defn- neighbours
  "Given a board and a cell return all neighbours of the cell"
  [board [x y _]]
  (remove nil?
          ; get a neighbour from each of the compass points
          [(cell board x (dec y))       ; N
           (cell board (inc x) (dec y)) ; NE
           (cell board (inc x) y)       ; E
           (cell board (inc x) (inc y)) ; SE
           (cell board x (inc y))       ; S
           (cell board (dec x) (inc y)) ; SW
           (cell board (dec x) y)       ; W
           (cell board (dec x) (dec y)) ; NW
           ]))

(defn- calculate-life
  "Given the current value of a cell and the count of all
  its live neighbours return if it should live 1, or die 0."
  [life-value live-neighbours]
  ; if a cell is dead and has 3 neighbours or alive and has 2 or 3 neighbours
  ; it lives, otherwise it dies
  (if (or (and (= "0" life-value)
               (= 3 live-neighbours)) ; dead and has 3 neighbours
          (and (= "1" life-value)
               (> 4 live-neighbours 1))) ; alive and has 2 or 3 live neighbours
    "1"
    "0"))

(defn age
  "Age a cell on the board, or the entire board"
  ([board]
   (reduce (fn [new-board cell]
             (conj new-board (age board cell)))
           []
           board))
  ([board cell]
   (let [[x y alive] cell
         live-neighbours (reduce (fn [life-force [_ _ alive?]]
                                   (+ life-force (Integer/parseInt alive?)))
                                 0
                                 (neighbours board cell))]
     [x y (calculate-life alive live-neighbours)])))

(defn string->board
  "Takes a board in string format and returns a seq of cell tuples.
   A cell tuple is [x y dead-or-alive]"
  [board-string]
  (->> (str/split board-string #"\s+")
       (map-indexed (fn [idx line]
                      (map-indexed #(list %1 idx (str %2)) line)))
       (apply concat)))

(defn board->string
  "Takes a board and returns a string - used for testing."
  [board cols]
  (->> board
       (map last)
       (partition cols)
       (map #(apply str %))
       (str/join " ")))

(defn print-board
  "Format and print a board"
  [board cols]
  (let [rows (->> board
                  (map last)
                  (partition cols)
                  (map #(apply str %)))]
    (doseq [row rows]
      (println row))))

;; Usage -----------------------------------------------------------------------

; from a repl
;   (require '[rally.conway :refer (string->board age print-board)])
; create a board
;   (def board (string->board "01000 10011 11001 01000 10001"))
; iterate and print
;   (print-board (age board) 5)
