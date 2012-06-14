(ns universal-clojure.core)

;; Welcome to Universal-Clojure. The idea behind this project is to provide
;; platform a platform-agnostic Clojure code parser. Firstly we should explain
;; that by "parser" we don't mean "text parser". No, that's technically a reader
;; or a lexer. This is a parser. The best way to show this is to give an example:

(comment

    (parse (with-meta '(+ a 1) {:line 42}))
    
    ;returns
    
    {:node-type :invoke
     :fn {:node-type :global-lookup
          :symbol '+}
     :args [{:node-type :global-lookup
             :symbol 'a}
            {:node-type :const
             :type :int
             :value 1}]
     :meta {:line 42}}
     
 )
;; So the over all point of this project is to provide all the syntax interpretation
;; in platform agnostic clojure code. The output of (parse) can be fed into a JSON
;; encoder (or any output) and then compiled to a new platform. What we've done
;; is de-complected the syntactic analysis from the VM code emission. 



;; These map predicates to keywords. 
(def ^:dynamic *tp-maps* {vector? :vector
                         nil? :nil
                         map? :map
                         seq? :seq
                         symbol? :symbol
                         number? :number
                         string? :string})

(defn merge-hash-set
    "Take several hash sets and merge them into a single set. There has to be a
    better way, but I can't seem to find it at the moment"
    [& sets]
    (apply hash-set (distinct (apply concat sets))))

(defn spec-parse 
    "Helper function for parsing arguments"
    [spec data]
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

(defn get-node-kw
    "For a given node, return a keyword. Why not use (type nd)? Well that isn't
    platform agnostic. Instead we want to unify all versions of maps under :map,
    all vectors should be :vectors, etc."
     [nd env]
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

(defmethod parse-node :string [nd env]
    {:node-type :const
     :data-type :string
     :value nd
     :meta (meta nd)})

(defmethod parse-node :vector [nd env]
    {:node-type :vector-literal
     :items (map parse nd (repeat env))
     :meta (meta nd)})

(defmethod parse-node :map [nd env]
    {:node-type :map-literal
     :items (map parse (mapcat identity nd) (repeat env))
     :meta (meta nd)})

(defn clean-map
    "Clears out all entries in the map who's values are either nil or empty
     we use this so that we can get rid of all sorts of garbage in the output"
     [mp]
     (select-keys mp (for [[k v] mp :when (not (filterable-key v))] k)))
     

(defn parse 
    ([nd]
     (parse nd {}))
    ([nd env]
     (let [result (parse-node (macroexpand nd) env)]
         (clean-map result))))

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

;; Resolves a symbol. If we're currently inside a quoted literal, then 
;; output a symbol literal. Otherwise decide if the symbol is a local
;; or a global
(defmethod parse-node :symbol [nd env]
    (let [locals (:locals env)]
         (cond (:quoted env)
                {:node-type :const 
                 :data-type :symbol
                 :value nd
                 :meta (meta nd)}
                (contains? locals nd)
                 (merge {:node-type :local
                         :meta (meta nd)
                         :used-locals #{nd}}
                        (get locals nd))
                :else
                 {:node-type :global-lookup
                  :value nd
                  :meta (meta nd)})))

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
  (into {} (for [[k v] m] 
         
              [k (f v)])))

(defn promote-closures [env]
    (let [closures (:locals env)]
         (assoc env 
                :locals
                (foreach-val (:locals env)
                            #(cons {:type :closure} %)))))

(defn add-locals [locals env]
    (let [locals (:locals env)
          locmerge (fn [locals loc]
                       (assoc locals
                              loc
                              (cons {:type :local} (get locals loc))))]
         (assoc locals 
                :locals
                (reduce locmerge locals))))                 
        
(defn make-fn-arg 
    "given a symbol and an existing env, create a local. Tag the given local
     with the argument offset. So for (fn [a b] a) a is tagged as offset 0, and
     b as 1"
    [sym offset env]
    {:local-type :fn-arg
     :name sym
     :offset offset})

(defn make-closure-arg
    "Make a closure that wraps the env local with the same name"
    [sym env]
    {:local-type :fn-arg
     :inner-arg (get-in env [:locals sym])
     :name sym})

(defn new-locals-from-fn-args
    "Given a argument list, create a new env that includes these args"
    [args env]
    (assoc env
           :locals
           (merge (:locals env)
               (into {} (map #(hash-map (get args %)
                                        (make-fn-arg (get args %) % env))
                              (range (count args)))))))

(defn parse-fn-body [form env]
    (let [[args & body] form
          [args & restarg] (split-with #(not (= '& %)) args)
          restarg (next (first restarg))
          last-is-rest (= (count restarg) 1)
          argsvec (vec (concat args restarg))
          newenv (new-locals-from-fn-args argsvec env)]
          (println restarg (count restarg))
         {:args argsvec
          :last-is-rest last-is-rest
          :body (parse-implicit-do body env)
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
           :forms (vec (map #(clean-map (parse-fn-body %1 %2)) (:rest sp) (repeat env)))
           :name (:name sp)
           :meta (meta form)}))

(defn parse-implicit-do [body env]
     (cond (nil? body) (parse nil env) ; default to nil
       (nil? (next body)) (parse (first body) env) ; optimize out this do
       :else
        (let [body (map parse body (repeat env))
              locals (apply merge-hash-set (map :used-locals body))]
             {:node-type :do
              :used-locals locals
              :body body})))

(defn parse-do [form env]
     (merge (parse-implicit-do (next form) env)
            {:meta (meta form)}))

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
            (let [args (map parse (next nd) (repeat env))]
                 {:node-type :invoke
                  :fn (parse (first nd) env)
                  :args args
                  :used-locals (apply merge-hash-set (map :used-locals args))
                  :meta (meta nd)}))))
              
(defn parsep [& x]
    (clojure.pprint/pprint (apply parse x)))

(parsep '(fn length [a b] (print "fo") (sqrt (* a a) (* b b))))
