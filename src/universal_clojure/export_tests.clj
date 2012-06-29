(ns universal-clojure.export-tests
  (:require [cheshire.core :as json]
            [universal-clojure.core :as core]
            [universal-clojure.test.test-data :as data]))

(defn parse-and-wrap [form]
  (-> form
      (core/parse-with-env)
      (first)
      (vector (eval form)
              )))


(defn process-data []
  (let [forms data/data
        wrapped (map parse-and-wrap forms)]
       (json/generate-string wrapped {:pretty true})))


(defn -main []
  (println (process-data)))
