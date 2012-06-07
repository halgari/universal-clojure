(ns universal-clojure.spec-parser)

(comment
    Spec Format
    [pred kw]
    
    )

(defn spec-parse [spec data]
    (loop [data data
           spec spec
           result {}]
          (let [curspec (first spec)]
              (cond (or (not data) (not spec)) result
                    (= (first spec) :rest) (assoc result :rest data)
                    ((first curspec) (first data)) 
                     (recur (next data) 
                            (next spec)
                            (assoc result (fnext (first spec))
                                          (first data)))
                    :default
                     (recur data
                            (next spec)
                            result)))))

(def test-spec [[symbol? :fn]
             [symbol? :name]
             [vector? :args]
             :rest])

(println (spec-parse test-spec '(fn name [x] 1 2 3)))
(println (spec-parse test-spec '(fn name ([x] 1 2 3) ([x y] 3))))
