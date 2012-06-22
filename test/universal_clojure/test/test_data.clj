(ns universal-clojure.test.test-data)


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
    (if false 1)])
