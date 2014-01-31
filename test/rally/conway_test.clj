(ns rally.conway-test
  (:require [clojure.test :refer :all]
            [rally.conway :as life]))

(deftest board-creation
  (testing "A board can be correctly initialized from a string"
    (is (= ['(0 0 "0") '(1 0 "1") '(2 0 "0") '(3 0 "0") '(4 0 "0")
            '(0 1 "1") '(1 1 "0") '(2 1 "0") '(3 1 "1") '(4 1 "1")
            '(0 2 "1") '(1 2 "1") '(2 2 "0") '(3 2 "0") '(4 2 "1")
            '(0 3 "0") '(1 3 "1") '(2 3 "0") '(3 3 "0") '(4 3 "0")
            '(0 4 "1") '(1 4 "0") '(2 4 "0") '(3 4 "0") '(4 4 "1")]
           (life/string->board "01000 10011 11001 01000 10001"))))
  (testing "A board can be correctly converted back to a string"
    (is (= "01000 10011 11001 01000 10001"
           (life/board->string
           ['(0 0 "0") '(1 0 "1") '(2 0 "0") '(3 0 "0") '(4 0 "0")
            '(0 1 "1") '(1 1 "0") '(2 1 "0") '(3 1 "1") '(4 1 "1")
            '(0 2 "1") '(1 2 "1") '(2 2 "0") '(3 2 "0") '(4 2 "1")
            '(0 3 "0") '(1 3 "1") '(2 3 "0") '(3 3 "0") '(4 3 "0")
            '(0 4 "1") '(1 4 "0") '(2 4 "0") '(3 4 "0") '(4 4 "1")]
           5)))))

(deftest generating-a-new-board
  (let [board (life/string->board "01000 10011 11001 01000 10001")]
    (testing "A new board is properly generated"
      (is (= "00000 10111 11111 01000 00000"
             (life/board->string (life/age board)
                                 5))))))

