

(require '[clojure.string :as str])
(defn execute
  [m s]
  (let [st (str/replace s #"\$\{\w+\}" "%s")
        sub-ks (map last (re-seq #"\$\{(\w+)\}" s))
        sub-vals (map m sub-ks)]
   (if (some nil? sub-vals)
     (let [missing (filter #(nil? (m %)) sub-ks)]
     (throw (Exception. (format "Missing key(s) => %s" (str/join ", " missing)))))
     (apply format st sub-vals))))

(execute
  {"day" "Thursday", "name" "Billy"}
  "${name} has an appointment on ${1day}, so please come ${lname}.")

(execute {"name" "Shashy"}
         "hello ${${name}}")
(map last

     (re-seq
       #"\$\{([\w|-]+)\}"
       "${f-name-l} has an appointment on ${Thursday}"
       )
     )

(str/replace
  "${name} has an appointment on ${Thursday}"
  #"\$\{\w+\}"
  "%s")


(some nil?
      (map {"name" "Shashy", "age" 42}
           ["name" "age" "height"])
      )

(filter #(nil? ({"name" "Shashy", "age" 42} %))
        ["name" "age" "height"])

(require '[clojure.test :refer [run-tests]])


(require '[rally.templating-engine :as te :reload true])
(require '[rally.templating-engine-test :reload true])
(run-tests 'rally.templating-engine-test)

(= "Missing keys(s) => age, gender"
   "Missing key(s) => age, gender")

(class :name)

