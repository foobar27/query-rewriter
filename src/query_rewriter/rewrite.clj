(ns query-rewriter.rewrite
  (:require [clojure.core.logic :as logic]))

;; # Overview

;; This namespace defines a term-rewriting system with the following features:
;; * statically typed
;; * automatic type-inference (via core.logic)
;; * configurable term rewriting strategies (via stratege)

;; The term-rewriting system consists of constructors and rules, which operate
;; on nested constructors. Both define sets of rules to be used by core.logic
;; for the type inference, and for the pattern matching.

;; TODO put into context:
;; At every call site of a function, the non-provided symbols need to be regenerated.


;; # Constructors

;; A constructor can take several logic symbols as arguments, required ones and optional ones.
;; * The first argument is the symbol which will be mapped to the return value.
;; * The required arguments are positional arguments and must be provided
;;   immediately after the return symbol.
;; * The optional arguments are named arguments and must be provided after the
;;   required ones.


(defmacro with-gensyms [symbols & body]
  (if (seq symbols)
    `(let [~@(apply concat (for [symbol symbols]
                             `[~symbol (gensym ~(str symbol))]))]
       ~@body)
    `(do
       ~body)))

(defn- split-args-and-locals [symbols]
  (let [[args [_ & locals]] (split-with #(not (= % :local)) symbols)]
    [args locals]))

(defn- binding->program [variable]
  (fn [ret]
    (cond
      (nil? variable)
      []

      (symbol? variable)
      `[(logic/== ~ret ~variable)]

      ;; function call
      (fn? variable)
      (variable ret)

      ;; constant
      true
      `[(logic/== ~ret ~variable)])))

(defmacro program-with-symbols [symbols & body]
  (let [[args locals] (split-args-and-locals symbols)
        statements (butlast body)
        return-statement (last body)
        ret-symbol (gensym "ret")
        args-symbols (into {}
                           (for [symbol args]
                             [symbol (gensym (str symbol "-argument"))]))]
    `(fn [~ret-symbol]
       (let [~@(apply concat (for [[s gs] args-symbols]
                               [gs s]))]
         (with-gensyms [~@(concat args locals)]
           `[(clojure.core.logic/fresh [~~@(concat args locals)]
               ~@(apply concat (for [[s# gs#] ~args-symbols]
                                 ((#'binding->program gs#) s#)))
               ~@~@statements
               ~@((#'binding->program ~return-statement) ~ret-symbol))])))))

(defn run-program [program]
  (let [ret (gensym "ret")]
    (eval
     `(clojure.core.logic/run* [~ret]
        ~@(program ret)))))

;; TODO rule macro with syntactic sugar?
(defn rule [left right]
  (program-with-symbols [left right]
    [::rule left right]))

(defn apply-rules [rules form]
  (let [ret (gensym "ret")]
    (eval
     `(clojure.core.logic/run* [output#]
        (logic/fresh [actual# ~ret]
          (logic/== ~ret [::rule ~form output#])
          (logic/conde
           ~@(for [rule rules]
               (vec (rule ret)))))))))

