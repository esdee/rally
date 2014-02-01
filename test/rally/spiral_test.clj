(ns rally.spiral-test
  (:require [clojure.test :refer :all]
            [rally.spiral :as sp]))

(deftest printing-the-spiral
  (testing "Spiral specified should print"
    (is (= (str " 20  21  22  23  24\n"
                " 19   6   7   8   9\n"
                " 18   5   0   1  10\n"
                " 17   4   3   2  11\n"
                " 16  15  14  13  12\n")
           (with-out-str (sp/print-spiral (sp/make-spiral 24)))))))
