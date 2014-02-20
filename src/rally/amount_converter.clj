;; Exercise 1
(ns rally.amount-converter
  (:require [clojure.string :as str]))

(def conversion-dictionary
  {:ones ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
   :tens ["ten" "eleven" "twelve" "thirteen" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]
   :units ["" "thousand" "million" "billion" "trillion" "i'll never know"]})

;; Convenience function to retrieve a value from the dictionary
;; based on the key and index
;; e.g. (from-dict :ones 1) => "one"
(defn- from-dict
  [ky idx]
  (get (ky conversion-dictionary) idx))

;; e.g.
;; (1 2) => twelve
;; (2 1) => "twenty one"
(defn- tens->string
  [tens ones]
  (let [i (Integer/parseInt (str tens ones))]
    (cond
      (< i 10) (from-dict :ones i)
      (< i 14) (from-dict :tens (- i 10))
      (= i 15) "fifteen" ; ooh English!
      (> 20 i 13) (format "%steen" (from-dict :ones (- i 10)) )
      :else (let [multiple-of-ten (int (Math/floor (/ i 10)))
                  ten-index (+ multiple-of-ten 2)
                  ones-index (mod i 10)]
              (format (if (= 0 ones-index) "%s%s" "%s-%s")
                      (from-dict :tens ten-index)
                      (from-dict :ones ones-index))))))

;; e.g.
;; ["thousands" (1)] => "one thousand",
;; ["" (1 2)] => twelve
(defn- unit-nums->string
  [[unit [ones tens hundreds]]]
  (str (when hundreds
         (str (from-dict :ones hundreds) " hundred "))
       (format "%s %s" (tens->string tens ones) unit)))

;; Converts a seq of dollars to a capitalized string
(defn- dollars->string
  [dollars]
  (when (seq dollars)
    (let [d-string (->> dollars ; ;[(3 2 1) (4 nil nil)]
                        (map-indexed #(list (from-dict :units %1) %2)) ; [(0 (3 2 1)) (1 (4 nil nil))]
                        (map unit-nums->string)
                        reverse
                        (str/join " "))]
      (str/trim (str/capitalize d-string)))))


;; Protocol used to humanize numbers ------------------------------------------
(defprotocol IHumanize
  (humanize [this]))

(extend-protocol IHumanize
  Object
  (humanize [this] (str this))

  Long
  (humanize [long]
    (->> (str long)
         str/reverse
         (map #(Integer/parseInt (str %)))
         (partition 3 3 nil) ; 123 => '((3 2 1))
         (dollars->string)))

  Integer
  (humanize [integer]
    (humanize (Long/parseLong (str integer))))

  clojure.lang.Ratio
  (humanize [ratio]
    (format "%02d/100" (Integer/parseInt (str (* ratio 100)))))

  Double
  (humanize [double]
    (let [dollars (long double)
          cents-as-ratio (/ (Math/round (* (mod double 1) 100)) 100)]
      (if (and (pos? dollars) (pos? cents-as-ratio))
        (str (humanize dollars) " and " (humanize cents-as-ratio))
        (if (pos? dollars) (humanize dollars) (humanize cents-as-ratio))))))

(defn amount->string
  "Given an amount as a number, convert it to a string representation"
  [amount]
  (str (humanize amount) " dollars"))
; Usage -----------------------------------------------------------------------

;  from the repl
;    (require '[rally.amount-converter :refer (amount->string)])
;    (amount->string 2523.04)
;    => "Two thousand five hundred twenty-three and 04/100 dollars"
