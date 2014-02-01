(ns rally.conway-test
  (:require [clojure.test :refer :all]
            [rally.conway :as life]))

(deftest board-creation
  (testing "A board can be correctly initialized from a string"
    (is (= {:rows 5
            :cols 5
            :cells [\0 \1 \0 \0 \0 \1 \0 \0 \1 \1 \1 \1 \0 \0 \1 \0 \1 \0 \0 \0 \1 \0 \0 \0 \1]}
           (life/string->board "01000 10011 11001 01000 10001"))))
  (testing "A board can be correctly converted back to a string"
    (is (= "01000 10011 11001 01000 10001"
           (life/board->string
            {:rows 5
            :cols 5
            :cells [\0 \1 \0 \0 \0 \1 \0 \0 \1 \1 \1 \1 \0 \0 \1 \0 \1 \0 \0 \0 \1 \0 \0 \0 \1]})))))

(deftest generating-a-new-board
  (let [board (life/string->board "01000 10011 11001 01000 10001")]
    (testing "A new board is properly generated"
      (is (= "00000 10111 11111 01000 00000"
             (life/board->string (life/age board)))))))

