;; Exercise 1
(ns rally.amount-converter
  (:require [clojure.string :as str]))


(def conversion-dictionary {
  :ones ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
  :tens ["ten" "eleven" "twelve" "thirteen" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]
  :units ["" "thousand" "million"]})

;; e.g.
;; (1 2) => twelve
;; (2 1) => "twenty one"
(defn- tens->string
  [tens ones]
  (let [i (Integer/parseInt (str tens ones))]
    (cond
      (< i 10) (get (:ones conversion-dictionary) i)
      (< i 14) (get (:tens conversion-dictionary) (- i 10))
      (= i 15) "fifteen" ; ooh English!
      (> 20 i 13) (format "%steen" (get (:ones conversion-dictionary) (- i 10)))
      :else (let [multiple-of-ten (int (Math/floor (/ i 10)))
                  ten-index (+ multiple-of-ten 2)
                  modulo (mod i 10)]
              (format (if (= 0 modulo) "%s%s" "%s-%s")
                      (get (:tens conversion-dictionary) ten-index)
                      (get (:ones conversion-dictionary) modulo))))))

;; e.g.
;; ["thousands" (1)] => "one thousand",
;; ["" (1 2)] => twelve
(defn- unit-nums->string
  [[unit [ones tens hundreds]]]
  (str (when hundreds
         (str (get (:ones conversion-dictionary) hundreds) " hundred "))
       (format "%s %s" (tens->string tens ones) unit)))

;; Converts a seq of dollars to a capitalizwed string
(defn- dollars->string
  [dollars]
  (when (seq dollars)
    (let [d-string (->> dollars
                        (map-indexed #(list (get (:units conversion-dictionary) %1)  %2))
                        (map unit-nums->string)
                        reverse
                        (str/join " "))]
      (->> [(str/capitalize (first d-string)) (rest d-string)]
           flatten
           (apply str)
           str/trim))))

;; Formats cents according to the spec, e.g 12 => "12/100", 4 => "04/100"
(defn- cents->string
  [cents]
  (when cents
    (format "%02d/100" (Integer/parseInt cents))))

(defn amount->string
  "Given an amount as a number, convert it to a string representation"
  [amount]
  (let [[d cents] (str/split (str amount) #"\.") ; split into dollars and cents
         dollars (->> (str/reverse d)
                     (map #(Integer/parseInt (str %)) )
                     (partition 3 3 nil))] ; 1234 => '((4 3 2) (1))
    (str
      (str/join
        " and "
        (remove empty? [(dollars->string dollars) (cents->string cents)]))
      " dollars")))

