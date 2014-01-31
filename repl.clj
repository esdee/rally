(defn update-cells
  [new-cells spiral]
  (let [removed (reduce (fn [s [x y _]]
                          (remove (fn [[sx sy _]]
                                    (and (= x sx) (= y sy)))
                                  s))
                  spiral
                  new-cells)]
    (concat removed new-cells)))

(defn right
  [steps [x y v]]
  (map (fn [i] [(+ x i 1) y (+ v i 1)])
       (range steps)))

(defn left
  [steps [x y v]]
  (map (fn [i] [(- x i 1) y (+ v i 1)])
       (range steps)))

(defn up
  [steps [x y v]]
  (map (fn [i] [x (- y i 1) (+ v i 1)])
       (range steps)))

(defn down
  [steps [x y v]]
  (map (fn [i] [x (+ y i 1) (+ v i 1)])
       (range steps)))

(defn even-iteration
  [current-cell cells-to-add]
  (let [[first-cell] (right 1 current-cell)  ; extend cols right
        col (down cells-to-add first-cell)
        row (left cells-to-add (last col))]
    (concat [first-cell] col row)))

(defn odd-iteration
  [current-cell cells-to-add]
  (let [[first-cell] (left 1 current-cell) ; extend cols left
        col (up cells-to-add first-cell)
        row (right cells-to-add (last col))]
    (concat [first-cell] col row)))

(defn intialize-blank-spiral
  [end]
  (let [cols (int (Math/ceil (Math/sqrt end)))
        rows (int (Math/ceil (/ end cols)))
        col0 (int (Math/floor (/ (dec cols) 2)))
        row0 (int (Math/floor (/ (dec rows) 2)))
        cell0 [(long col0) (long row0) 0]] ; cast for comparison sorting later
    (->> (map (fn [point] (conj point nil))
              (for [x (range 0 cols)
                    y (range 0 rows)]
                [x y]))
         (update-cells [cell0]))))

(defn sort-spiral-for-printing
  [spiral]
  (sort (fn [[x1 y1 _] [x2 y2 _]]
          (if (= y1 y2)
            (.compareTo x1 x2)
            (.compareTo y1 y2)))
        spiral))

(defn make-spiral
  [i]
  (let [end (inc i)]
    (loop [spiral (intialize-blank-spiral end)
           even-iteration? true
           add 1]
      (let [last-cell (last spiral)
            iteration-fn (if even-iteration? even-iteration odd-iteration)]
        (if (<= i (last last-cell))
          spiral
          (let [new-cells (iteration-fn last-cell add)
                new-spiral (update-cells new-cells spiral)]
            (recur new-spiral (not even-iteration?) (inc add))))))))

(defn print-spiral
  [spiral]
  (let [spiral (sort-spiral-for-printing spiral)
        cols (int (Math/sqrt (count spiral)))]
    (doseq [row (partition cols spiral)]
      (->> (map last row)
           (map #(format "%3d" %))
           (str/join " ")
           println)
    spiral)))

(print-spiral (make-spiral 24))
