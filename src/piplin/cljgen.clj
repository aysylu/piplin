(ns piplin.cljgen
  (:require piplin.core)
  (:require [piplin.walk :as walk])
  (:use piplin.types)
  (:refer-clojure :exclude [cast]))

(+ 3 3)

(-> (+ 3 3) (bit-and 0xf))

(defn merged-args
  [ast]
  (apply merge (map (value ast) [:args :consts])))

(defmulti ast->clj
  "Converts Piplin AST to Clojure AST"
  (fn [ast _]
    (if (pipinst? ast)
      :pipinst
      (-> ast value :op)))
  )

(defmethod ast->clj
  :default
  [ast name-lookup]
  (throw (RuntimeException. (str "Cannot convert clojure: " ast))))

(defmethod ast->clj
  :pipinst
  [ast name-lookup]
  (println "this is piplin type " ast)
  (value ast))

(defmethod ast->clj
  :noop
  [ast name-lookup]
  (let [args (merged-args ast)]
    (name-lookup (:expr args))))

(defmethod ast->clj
  :+
  [ast name-lookup]
  (let [{lhs :lhs rhs :rhs}
        (merged-args ast)]
    (list 'piplin.core/+
          (name-lookup lhs)
          (name-lookup rhs))))

(defn expr->name+binding
  "This function takes an AST fragment and returns a name
  for the expression and a pair to be put into a let
  binding that computes the expression."
  [expr name-lookup]
  (let [name (gensym)
        value (ast->clj expr name-lookup)]
    [name [name value]]))

(defn make-let-bindings
  "This function takes an AST fragment and creates a let
  binding that can be `eval`ed to compute the value of the
  AST fragment."
  [expr name-table]
  (let [[name-table body] (walk/compile expr expr->name+binding name-table [])]
    (apply concat body))
  )

(defn make-scaffolding
  "Takes a seq of argument names, a seq of bindings,
  and the name of the result and returns an `eval`able
  form for that function."
  [args bindings result]
  (list `fn (vec args)
        (list `let (vec bindings)
              result)))

(defn cljgen-expr
  "Takes an expr and returns an `eval`able form"
  [expr]
  (let [name-table {}
        bindings (make-let-bindings expr name-table)
        result (-> bindings butlast last)]
    (eval (make-scaffolding [] bindings result))))

((cljgen-expr (piplin.core/+ 3 (piplin.types/uninst ((piplin.core/uintm 4) 4))                                    )))
 (clojure.pprint/pprint (piplin.core/+ 3 (piplin.types/uninst ((piplin.core/uintm 4) 4))                                    ))


((eval (list `fn [] (list `let '[G__3988 4 G__3989 G__3988 G__3990 3 G__3991 (piplin.core/+ G__3990 G__3989)]
   'G__3991))))

