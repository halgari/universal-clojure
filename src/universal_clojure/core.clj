(ns universal-clojure.core)


(def ^:dynamic *tp-maps* {vector? :vector
                         nil? :nil
                         map? :map
                         seq? :seq
                         symbol? :symbol})

(defn get-node-kw [nd env]
    """For a given node, return a keyword. Why not use (type nd)? Well that isn't
    platform agnostic. Instead we want to unify all versions of maps under :map,
    all vectors should be :vectors, etc."""
    (second (first (filter (fn [[k v]] (k nd)) *tp-maps*))))
                         

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
    (let [result (parse-node nd env)]
         (select-keys result
                      (for [[k v] result :when (not (filterable-key v))] k))))

(defmethod parse-node :seq [nd env]
    (if (:quoted env)
        {:node-type :seq-literal
         :items (map parse nd (repeat env))
         :meta (meta nd)}
        (parse-invoke (first nd) (rest nd) env)))
    
