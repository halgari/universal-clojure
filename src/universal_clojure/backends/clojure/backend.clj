(ns universal-clojure.backends.clojure.backend)

;; This namespace houses the reference compiler for Clojure. This compiler is fairly useless but will help to
;; provide a reference implementation that other compilers can use for verification and testing


;; The main compiler functions dispatch on node-type
(defmulti compile-node (fn [node env] (:node-type node)))

;; Dispatch consts on data-type
(defmulti compile-const :data-type)

;; Link them up
(defmethod compile-node :const [node env]
  (compile-const node env))

(defmethod compile-const :string
  [node env]
  (str (:value node)))

(defmethod compile-const :nil
  [node env]
  nil)

(defmethod compile-const :number
  [node env]
  (:value node))

(defmethod compile-const :bool
  [node env]
  (:value node))

(defmethod compile-node :vector-literal [node env]
  (vec (map #(compile-node % env) (:items node))))

(defmethod compile-node :if [node env]
  `(if ~(compile-node (:cond node) env)
     ~(compile-node (:then node) env)
     ~(compile-node (:else node) env)))

(defmethod compile-node :do [node env]
  `(do ~@(map compile-node (:body node) (repeat env))))

;; Main entry function
(defn compile
  ([ast] (compile-node ast {}))
  ([ast env] (compile-node ast env)))
