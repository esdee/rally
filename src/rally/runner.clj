(ns rally.runner
  (:require [clojure.string :as str]
            [rally.amount-converter :refer (amount->string)]
            [rally.spiral :refer (make-spiral print-spiral)]
            [rally.conway :refer (age board->string string->board print-board)]
            [rally.templating-engine :refer (template->string)])
  (:gen-class))

(defn -main
  [& _]
  (println "Running Exercises")
;-------------------------------------------------------------------------------
  (println "Exercise 1: Amount Converter")
  (println "============================")
  (println (format "2523.04 => %s" (amount->string 2523.04)))
  (println)
;-------------------------------------------------------------------------------
  (println "Exercise 3: Spiral")
  (println "==================")
  (print-spiral (make-spiral 24))
  (println)
;-------------------------------------------------------------------------------
  (println "Exercise 4: Game Of Life")
  (println "========================")
  (let [board0 (string->board"01000 10011 11001 01000 10001")]
    (println (format "Original board"))
    (print-board board0 5)
    (println (format "Board generation 1"))
    (print-board (age board0) 5))
  (println)
;-------------------------------------------------------------------------------
  (println "Exercise 5: Templating Engine")
  (println "=============================")
  (println "Template \"${name} has an appointment on ${day}\"")
  (println "With map {\"name\" \"Billy\", \"day\" \"Thursday\"}")
  (println (template->string
             "${name} has an appointment on ${day}"
             {"name" "Billy" "day" "Thursday"}))
;-------------------------------------------------------------------------------
  (System/exit 0))