(ns universal-clojure.core
  (:refer-clojure :exclude  [macroexpand-1])
  (:require [universal-clojure.common.macros :as macros]))

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

(defn debug [p]
   (println p)
   p)


(defn cache-macros []
  (zipmap (map symbol (keys (ns-publics 'universal-clojure.common.macros)))
          (vals (ns-publics 'universal-clojure.common.macros))))

(def macros (cache-macros))

(defn macro? [f]
  (:macro (meta f)))

(defn macroexpand-1 [[ft & rst] env]
  (let [m (get macros ft)]
    (if (macro? m)
      (apply m nil env rst)
      (cons ft rst))))

;; Forward def for the body of a do
(def parse-implicit-do)

;; compiler intrinsics are functions that create AST nodes. Examples of this are
;; fn*, let*, etc.
(def ^:dynamic *compiler-intrinsics* (atom {}))

;; Forward def for parsing of invokes
(def parse-invoke)


;; Main entry point for this library passing a form to this function will return
;; the parse AST

(def parse)


(defn bool? [nd]
  (or (= nd true) (= nd false)))

;; These map predicates to keywords.
(def ^:dynamic *tp-maps* {vector? :vector
                         nil? :nil
                         map? :map
                         seq? :seq
                         symbol? :symbol
                         number? :number
                          string? :string
                          bool? :bool})


(defn get-node-kw
    "For a given node, return a keyword. Why not use (type nd)? Well that isn't
    platform agnostic. Instead we want to unify all versions of maps under :map,
    all vectors should be :vectors, etc."
     [nd env]
    (let [kw (second (first (filter (fn [[k v]] (k nd)) *tp-maps*)))]
         (if kw
             kw
             (throw (Exception. (str "Unknown Node type " (type nd)))))))


;; Here we can install new node parsers this will dispatch on the type of the node
;; so passing in nil will dispatch using :nil, a vector will get passed to :vector
;; etc.
(defmulti parse-node get-node-kw)


(defn merge-hash-set
    "Take several hash sets and merge them into a single set. There has to be a
    better way, but I can't seem to find it at the moment"
    [& sets]
    (apply hash-set (distinct (apply concat sets))))


(defmacro defintrinsic [name & body]
    `(swap! *compiler-intrinsics* assoc (quote ~name)
            (fn ~name ~@body)))

(defn spec-parse
  "Helper function for parsing arguments. The idea here is that you can pass in a list of
   predicates and keywords. This function tries to match each item in the seq to a keyword
   by using a predicate. If the predicate fails, the curren item being matched falls through
   to the next predicate. It's a bit hard to explain, so check out the use cases in this file
   for a better example"
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



(defn filterable-key [k]
    (cond (nil? k) true
          (and (seq? k) (empty? k)) true
          :default false))

;; Parse nodes of each type.

;; nil gets parsed as a const
(defmethod parse-node :nil [nd env]
    {:node-type :const
     :data-type :nil
     :meta (meta nd)})

;; strings are consts
(defmethod parse-node :string [nd env]
    {:node-type :const
     :data-type :string
     :value nd
     :meta (meta nd)})

;; TODO: extend this to check for being inside a quote
;; Parse this as a vector node, and parse our children
(defmethod parse-node :vector [nd env]
    {:node-type :vector-literal
     :items (map parse nd (repeat env))
     :meta (meta nd)})

;; Same as vectors, but for maps
(defmethod parse-node :map [nd env]
    {:node-type :map-literal
     :items (map parse (mapcat identity nd) (repeat env))
     :meta (meta nd)})

(defn clean-map
    "Clears out all entries in the map who's values are either nil or empty
     we use this so that we can get rid of all sorts of garbage in the output"
     [mp]
     (select-keys mp (for [[k v] mp :when (not (filterable-key v))] k)))


(defn make-env []
  "Create a default environment. We use a atom here so that child nodes can
   define global defs."
    {:namespace (atom {})})

(defn parse-with-env
    "parses nd with a new environment"
    ([nd]
     (let [env (make-env)]
          [(parse nd env) env])))

(defn parse
    ([nd env]
     (let [result (parse-node nd env)]
         (clean-map result))))


(defmethod parse-node :seq [nd env]
    (if (:quoted env)
        {:node-type :seq-literal
         :items (map parse nd (repeat env))
         :meta (meta nd)}
        (parse-invoke (macroexpand-1 nd env) env)))

(defmethod parse-node :keyword [nd env]
    {:node-type :const
     :data-type :keyword
     :value nd
     :meta (meta nd)})

;; numbers are consts
(defmethod parse-node :number [nd env]
    {:node-type :const
     :data-type :number
     :value nd
     :meta (meta nd)})

(defmethod parse-node :bool [nd env]
  {:node-type :const
   :data-type :bool
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


;; Compiler intrinsic if. We tag each form as cond, then or else
(defintrinsic if [form env]
    (let [[_ cond then & else] form]
        {:node-type :if
         :cond (parse cond env)
         :then (parse then env)
         :else (if else (parse (first else) env) (parse nil env))
         :meta (meta form)}))


(defintrinsic ns [form env]
    (swap! (:namespace env)
           merge
           {:name (next form)}))



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
          newenv (new-locals-from-fn-args argsvec env)
          body (parse-implicit-do body newenv)]
          {:args argsvec
           :last-is-rest last-is-rest
           :body body
           :used-locals (:used-locals body)
           :required-arity (count args)
           :rest-arg (when last-is-rest (first restarg))
           :meta (meta form)}))

(defintrinsic fn* [form env]
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
        :else (let [body (map parse body (repeat env))
                    locals (apply merge-hash-set (map :used-locals body))]
                {:node-type :do
                 :used-locals locals
                 :body body})))

(defintrinsic do [form env]

     (merge (parse-implicit-do (next form) env)
            {:meta (meta form)}))

(defn is-intrinsic? [n]
     (contains? @*compiler-intrinsics* n))

(defn parse-intrinsic [nd env]
    ((get @*compiler-intrinsics* (first nd)) nd env))

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

    (println (apply parse-with-env x)))
