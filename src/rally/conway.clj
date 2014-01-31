(ns rally.conway
  (:require [clojure.string :as str]))


(defn string->board
  "Takes a board in string format and returns a seq of cell tuples.
   A cell tuple is [x y dead-or-alive]"
  [board-string]
  (->> (str/split board-string #"\s+")
       (map-indexed (fn [idx line]
                      (map-indexed #(list %1 idx (str %2)) line)))
       (apply concat)))

(defn board->string
  [board cols]
  (->> board
       (map last)
       (partition cols)
       (map #(apply str %))
       (str/join " ")))

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
          [(cell board x (dec y))       ; 12 o'clock
           (cell board (inc x) (dec y)) ; 1
           (cell board (inc x) y)       ; 3
           (cell board (inc x) (inc y)) ; 5
           (cell board x (inc y))       ; 6
           (cell board (dec x) (inc y)) ; 8
           (cell board (dec x) y)       ; 9
           (cell board (dec x) (dec y)) ; 11
           ]))

(defn- calculate-life
  "Given the current value of a cell and the count of all
  its live neighbours return if it should live, 1 or die 0."
  [current surrounding]
  (if (or (and (= "0" current)
               (= 3 surrounding))
          (and (= "1" current)
               (> 4 surrounding 1)))
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
         life-force (reduce (fn [life-force [_ _ alive?]]
                              (+ life-force (Integer/parseInt alive?)))
                            0
                            (neighbours board cell))]
     [x y (calculate-life alive life-force)])))
