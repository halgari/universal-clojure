(ns universal-clojure.test.core
  (:require [universal-clojure.core :as core]
            [universal-clojure.backends.clojure.backend :as backend]
            [ universal-clojure.test.test-data :as data])
  (:use [clojure.test]))


(defn debug [data]
  (println data)
  data)

(defn compile-and-run [form]
  (eval (backend/compile (debug (first (core/parse-with-env form))))))


(deftest test-all
  (doseq [f data/data]
    (let [r (eval f)]
      (println f)
      (is (=  r (compile-and-run f))))))
