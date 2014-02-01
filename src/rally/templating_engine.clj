;; Exercise 5
(ns rally.templating-engine
  (:require [clojure.string :as str]))

;; This is the regex used to replace the template variables
;; of the for ${x}
(def ^:private regex #"\$\{([\w|-]+)\}")

(defn template->string
  "Given a template string and a variable map, substitute each appearance of a
  matcing key in the string template with the corresponding value from the
  variable map. If a key is referenced by the template but missing in the map,
  will throw an Exception"
  [template-string variable-map]
  (let [keys-in-template (map last (re-seq regex template-string))
        vals-for-template (map variable-map keys-in-template)]
    (if (some nil? vals-for-template)
      (let [missing-keys (filter #(nil? (variable-map %)) keys-in-template)]
        (throw (Exception. (format "Missing key(s) => %s"
                                    (str/join ", " missing-keys))))))
    (apply format
           (str/replace template-string regex "%s")
           vals-for-template)))

