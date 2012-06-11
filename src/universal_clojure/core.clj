(ns universal-clojure.core)


(def ^:dynamic *tp-maps* {vector? :vector
                         nil? :nil
                         map? :map
                         seq? :seq
                         symbol? :symbol
                         number? :number})

(defn spec-parse [spec data]
    """Helper function for parsing arguments"""
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

(defn get-node-kw [nd env]
    """For a given node, return a keyword. Why not use (type nd)? Well that isn't
    platform agnostic. Instead we want to unify all versions of maps under :map,
    all vectors should be :vectors, etc."""
    (let [kw (second (first (filter (fn [[k v]] (k nd)) *tp-maps*)))]
         (if kw 
             kw
             (throw (Exception. (str "Unknown Node type " (type nd)))))))
                         

(defn filterable-key [k]
    (cond (nil? k) true
          (and (seq? k) (empty? k)) true
          :default false))

(defmulti parse-node get-node-kw)

(def parse)

(defmethod parse-node :nil [nd env]
    {:node-type :const
     :data-type :nil
     :meta (meta nd)})

(defmethod parse-node :vector [nd env]
    {:node-type :vector-literal
     :items (map parse nd (repeat env))
     :meta (meta nd)})

(defmethod parse-node :map [nd env]
    {:node-type :map-literal
     :items (map parse (mapcat identity nd) (repeat env))
     :meta (meta nd)})

(defn parse 
    ([nd]
     (parse nd {}))
    ([nd env]
     (let [result (parse-node (macroexpand nd) env)]
         (select-keys result
                      (for [[k v] result :when (not (filterable-key v))] k)))))

(def parse-invoke)

(defmethod parse-node :seq [nd env]
    (if (:quoted env)
        {:node-type :seq-literal
         :items (map parse nd (repeat env))
         :meta (meta nd)}
        (parse-invoke nd env)))

(defmethod parse-node :keyword [nd env]
    {:node-type :const
     :data-type :keyword
     :value nd
     :meta (meta nd)})

(defmethod parse-node :symbol [nd env]
    (if (:quoted env)
        {:node-type :const 
         :data-type :symbol
         :value nd
         :meta (meta nd)}
        {:node-type :symbolic-env-lookup
         :value nd
         :meta (meta nd)}))

(defmethod parse-node :number [nd env]
    {:node-type :const
     :data-type :number
     :value nd
     :meta (meta nd)})


(defn parse-if [form env]
    (let [[_ cond then & else] form]
        {:node-type :if
         :cond (parse cond env)
         :then (parse then env)
         :else (if else (parse else env) (parse nil env))
         :meta (meta form)}))

(defn debug [p]
   (println p)
   p)

(defn foreach-val [m f]
  (into {} (debug (for [[k v] m] 
         
              [k (f v)]))))

(defn promote-closures [env]
    (let [closures (:locals env)]
         (assoc env 
                :locals
                (foreach-val (:locals env)
                            #(cons {:type :closure} %)))))
        
    

(defn parse-fn-body [form env]
    (let [[args & body] form
          [args & restarg] (split-with #(not (= '& %)) args)
          restarg (next (first restarg))
          last-is-rest (= (count restarg) 1)]
          (println restarg (count restarg))
         {:args (vec (concat args restarg))
          :last-is-rest last-is-rest
          :body (map parse body (repeat env))
          :required-arity (count args)
          :rest-arg (when last-is-rest (first restarg))
          :meta (meta form)}))
    
(defn parse-fn* [form env]
    (let [sp (spec-parse [[symbol? :fn]
                          [symbol? :name]
                          [vector? :args]
                          :rest]
                         form)
          sp (if (:args sp)
                 (assoc sp :rest [(cons (:args sp)
                                        (:rest sp))])
                 sp)
          sp (if (:name sp)
                 sp
                 (assoc sp :name (gensym "fn")))]
          {:node-type :fn
           :forms (vec (map parse-fn-body (:rest sp) (repeat env)))
           :name (:name sp)
           :meta (meta form)}))
           

(def ^:dynamic *compiler-intrinsics*
    {'if parse-if
     'fn* parse-fn*})

(defn is-intrinsic? [n]
     (contains? *compiler-intrinsics* n))

(defn parse-intrinsic [nd env]
    ((get *compiler-intrinsics* (first nd)) nd env))

(defn parse-invoke [nd env]
    (let [n (first nd)]
        (if (and (symbol? n) (is-intrinsic? n)) 
            (parse-intrinsic nd env)
            {:node-type :invoke
             :fn (parse (first nd) env)
             :args (map parse (next nd) (repeat env))
             :meta (meta nd)})))
              
(defn parsep [& x]
    (clojure.pprint/pprint (apply parse x)))
    
