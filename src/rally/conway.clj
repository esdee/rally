(ns rally.conway
  (:require [clojure.string :as str]))


(defn cell
  "Given a board and x, y coords, return the cell at that position.
  Will raise an IndexOutOfBoundsException with incorrect parameters"
  [{:keys [rows cols cells]} x y]
  (nth cells (+ (* y cols) x)))

(defn neighbours
  "Given a board and a cell return all neighbours of the cell.
  Neighbours are checked along the 8 compass points N NE E SE S SW W NW"
  [{:keys [rows cols]:as board} x y]
  (let [valid-coord? #(and (> %1 -1) (> %2 -1) (< %1 cols) (< %2 rows))
                        ;N       NE     E    SE
        compass-points [[0 -1] [1 -1] [1 0] [1 1]
                       ; S      SW      W     NW
                        [0 1] [-1 1] [-1 0] [-1 -1]]]
    (remove nil?
            (map (fn [[x-delta y-delta]]
                   (let [neighbour-x (+ x x-delta)
                         neighbour-y (+ y y-delta)]
                     (when (valid-coord? neighbour-x neighbour-y)
                       (cell board neighbour-x neighbour-y))))
                 compass-points))))

(defn calculate-life
  "Given the current value of a cell and the count of all
  its live neighbours return if it should live 1, or die 0."
  [life-value live-neighbours]
  ; if a cell is dead and has 3 neighbours or alive and has 2 or 3 neighbours
  ; it lives, otherwise it dies
  (if (or (and (= \0 life-value)
               (= 3 live-neighbours)) ; dead and has 3 neighbours
          (and (= \1 life-value)
               (> 4 live-neighbours 1))) ; alive and has 2 or 3 live neighbours
    \1
    \0))

(defn age
  "Age a cell on the board, or the entire board"
  ([{:keys [rows cols] :as board}]
   (let [cell-coords (for [y (range rows)
                           x (range cols)]
                       [x y]) ; all possible coords on board
         ]
     (assoc board
       :cells (reduce (fn [cells [x y]]
                        (conj cells (age board x y)))
                      []
                      cell-coords))))
  ([board x y]
   (let [live-neighbours (reduce (fn [life-force neighbour]
                                   (+ life-force (Integer/parseInt (str neighbour))))
                                 0
                                 (neighbours board x y))]
     (calculate-life (cell board x y) live-neighbours))))

(defn string->board
  "Takes a board in string format and returns a
   map {:rows count-of-board-rows
        :cols count of cols per row
        :cells flat seq of cells 0s and 1s }"
  [board-string]
  (let [rows (str/split board-string #"\s+")]
    {:rows (count rows)
     :cols (count (first rows))
     :cells (reduce (fn [cells row]
                      (concat cells (vec row)))
                    []
                    rows)}))

(defn board->string
  "Takes a board and returns a string - used for testing."
  [{:keys [cols cells]}]
  (->> cells
       (partition cols)
       (map #(apply str %))
       (str/join " ")))

(defn print-board
  "Format and print a board"
  [board]
  (doseq [row (str/split (board->string board) #"\s+")]
    (println row)))

;; Usage -----------------------------------------------------------------------

; from a repl
;   (require '[rally.conway :refer (string->board age print-board)])
; create a board
;   (def board (string->board "01001 10011 11001 01000 10001"))
; iterate and print
;   (print-board (age board))
