(ns universal-clojure.test.test-data)


;; Here we define some test data. export_tests.clj will take this
;; data and export it as a vector of [form, result]. This then defines
;; a contract that clients can use to validates their backends. This
;; contract can be found in test_data.json. Each backend then need only
;; compile and eval the form and then test it with the result as defined
;; in the json file. So try to make this data return a platform agnostic
;; datastructure

(def data
  '[1
   2
   nil
   "foo"
   (do)
   (do 1)
   (do 1 2 3)
   []
   [1]
   [1 2 3 4 5 6 7 8 8 11]
    true
    false
    (if true 1 0)
    (if false 1 2)
    (if true 1)
    (if false 1)

    ;; fn defs
    ((fn* [x y] x) 1 2)
    ((fn* [x y] y) 1 2)])
