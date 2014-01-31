(ns rally.templating-engine-test
  (:require [clojure.test :refer :all]
            [rally.templating-engine :as te]))

(deftest template->string
  (testing "Substitution of single instance of keys"
    (is (= "Billy has an appointment on Thursday"
           (te/template->string {"name" "Billy", "day" "Thursday"}
                                "${name} has an appointment on ${day}"))))

  (testing "Substitution of multiple instances of keys"
    (is (= "Full Name: Billy Smith. First Name: Billy, Last Name: Smith."

           (te/template->string
             {"fname" "Billy", "lname" "Smith"}
             "Full Name: ${fname} ${lname}. First Name: ${fname}, Last Name: ${lname}."))))

  (testing "An exception is thrown when keys are referenced in the template but
            missing in the variable map"
    (is (thrown? Exception
                 (te/template->string {"name" "Billy"} "${name} ${age}"))))

  (testing "The exception thrown referenced keys in the variable map are missing
             should list the keys that are missing"
    (let [exception (try (te/template->string {"name" "Billy"}
                                              "${name} ${age} ${gender}")
                         (catch Exception e
                           e))]
      (is (= "Missing key(s) => age, gender" (.getMessage exception)))))

  (testing "Correctly handles dashed keys"
    (is (= "Billy" (te/template->string {"employee-first-name" "Billy"}
                                        "${employee-first-name}")))))