(ns universal-clojure.core)


(def ^:dynamic *tp-maps* {vector? :vector
                         nil? :nil
                         map? :map
                         seq? :seq
                         symbol? :symbol
                         number? :number})

(defn get-node-kw [nd env]
    """For a given node, return a keyword. Why not use (type nd)? Well that isn't
    platform agnostic. Instead we want to unify all versions of maps under :map,
    all vectors should be :vectors, etc."""
    (let [kw (second (first (filter (fn [[k v]] (k nd)) *tp-maps*)))]
         (if kw 
             kw
             (throw (Exception. (str "Unknown Node type " nd))))))
                         

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

(defn parse [nd env]
    (let [result (parse-node (macroexpand nd) env)]
         (select-keys result
                      (for [[k v] result :when (not (filterable-key v))] k))))

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
    

(def ^:dynamic *compiler-intrinsics*
    {'if parse-if})

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
              
    
    
