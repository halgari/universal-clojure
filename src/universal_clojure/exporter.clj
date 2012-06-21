(ns universal-clojure.exporter
  (:use [ cheshire.core])
  (:require [universal-clojure.core :as core])
  (:gen-class :main :true))

(defn process-file [file]
  (-> (str "[" (slurp file) "]")
      (read-string)
      (core/parse-with-env)
      (first)
      (generate-string {:pretty true})))

(defn -main [args]
  (println (process-file args)))
