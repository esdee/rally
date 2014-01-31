(ns rally.templating-engine
  (:require [clojure.string :as str]))


(defn template->string
  "Given a variable map and a string template, substitute each appearance of a
  matcing key in the string template with the corresponding value from the
  variable map. If a key is referenced by the template but missing in the map,
  will throw an Exception"
  [variable-map string-template]
  (let [keys-in-template (map last (re-seq #"\$\{([\w|-]+)\}" string-template))
        vals-for-template (map variable-map keys-in-template)]
    (if (some nil? vals-for-template)
      (let [missing-keys (filter #(nil? (variable-map %)) keys-in-template)]
        (throw (Exception. (format "Missing key(s) => %s"
                                    (str/join ", " missing-keys))))))
    (apply format
           (str/replace string-template #"\$\{[\w|-]+\}" "%s")
           vals-for-template)))

