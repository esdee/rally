(ns rally.templating-engine-test
  (:require [clojure.test :refer :all]
            [rally.templating-engine :as te]))

(deftest template->string
  (testing "Substitution of single instance of keys"
    (is (= "Billy has an appointment on Thursday"
           (te/template->string "${name} has an appointment on ${day}"
                                {"name" "Billy", "day" "Thursday"} ))))

  (testing "Substitution of multiple instances of keys"
    (is (= "Full Name: Billy Smith. First Name: Billy, Last Name: Smith."
           (te/template->string
             "Full Name: ${fname} ${lname}. First Name: ${fname}, Last Name: ${lname}."
             {"fname" "Billy", "lname" "Smith"} ))))

  (testing "An exception is thrown when keys are referenced in the template but
            missing in the variable map"
    (is (thrown? Exception
                 (te/template->string "${name} ${age}" {"name" "Billy"}))))

  (testing "Strings can be escaped"
    (is (= "hello ${Billy}"
           (te/template->string "hello ${${name}}" {"name" "Billy"}))))

  (testing "The exception thrown referenced keys in the variable map are missing
             should list the keys that are missing"
    (let [exception (try (te/template->string "${name} ${age} ${gender}"
                                              {"name" "Billy"} )
                         (catch Exception e
                           e))]
      (is (= "Missing key(s) => age, gender" (.getMessage exception)))))

  (testing "Correctly handles dashed keys"
    (is (= "Billy" (te/template->string "${employee-first-name}"
                                        {"employee-first-name" "Billy"} )))))