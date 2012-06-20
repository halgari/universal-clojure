(ns universal-clojure.test.core
  (:require [universal-clojure.core :as core]
            [universal-clojure.backends.clojure.backend :as backend])
  (:use [clojure.test]))


(defn debug [data]
  (println data)
  data)

(defmacro comp-test [name form]

  `(deftest ~name
     (println ~(clojure.core/name name))
     (is true
         (= (eval (backend/compile (debug (first (core/parse-with-env ~form)))))
            ~form))))


(comp-test nil-is-nil nil)
(comp-test string-is-string "foo")
(comp-test number-is-number 1)

(comp-test vector-is-empty [])
(comp-test vector-is-single [1])
(comp-test long-vector (vec (range 100)))

(comp-test empty-do (do))
(comp-test single-do (do 1))
(comp-test multi-do (do 1 2 3))
