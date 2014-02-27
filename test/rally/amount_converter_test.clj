(ns rally.amount-converter-test
  (:require [clojure.test :refer :all]
            [rally.amount-converter :as ac :reload true]))

(deftest amount->string
  (testing "The number to convert as per the spec"
    (is (= "Two thousand five hundred twenty-three and 04/100 dollars"
           (ac/amount->string 2523.04))))

  (testing "Dollar values only"
    (is (= "One hundred dollars"
           (ac/amount->string 100)))
    (is (= "Twelve dollars"
         (ac/amount->string 12)))
    (is (= "Twelve dollars"
         (ac/amount->string 12)))
    (is (= "One dollars"
         (ac/amount->string 1))))

  (testing "English language rule exceptions"
    (is (= ["Eleven dollars" "Twelve dollars" "Thirteen dollars" "Fifteen dollars"]
           (map ac/amount->string [11 12 13 15]))))

  (testing "Cents only"
    (is (= "12/100 dollars"
           (ac/amount->string 0.12))))

  (testing "I'm gonna pop some tags"
    (is (= "Twenty dollars" ; not Twenty-
         (ac/amount->string 20)))))
