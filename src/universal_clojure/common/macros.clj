(ns universal-clojure.common.macros)

(defmacro when [cond & exprs]
  `(if ~cond (do ~@exprs) nil))
