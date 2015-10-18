(ns query-rewriter.category
  (:require [query-rewriter.rewrite :as rewrite :refer [program-with-symbols rule run-program apply-rules]]))

;; TODO defn category

(defn object [& {:keys [category value]}]
  (program-with-symbols [category value]
    [::object category value]))

(defn morphism [& {:keys [category source target value]}]
  (program-with-symbols [category source target value]
    ((object :category category) source)
    ((object :category category) target)
    [::morphism category source target value]))

(defn identity [& {:keys [category object]}]
  (program-with-symbols [category object]
    (morphism :source object :target object :category category :value ::identity)))

(defn compose [m1 m2]
  (program-with-symbols [m1 m2 :local o1 o2 o3]
    ((morphism :source o1 :target o2) m2)
    ((morphism :source o2 :target o3) m1)
    (morphism :source o1 :target o3 :value [::compose m1 m2])))

(defn left-identity-rule []
  (program-with-symbols [:local f]
    (rule (compose (identity) f)
          f)))

(defn right-identity-rule []
  (program-with-symbols [:local f]
    (rule (compose f (identity))
          f)))

(defn associativity-rule-1 []
  (program-with-symbols [:local m1 m2 m3]
    (rule (compose m3 (compose m2 m1))
          (compose (compose m3 m2) m1))))

(defn associativity-rule-2 []
  (program-with-symbols [:local m1 m2 m3]
    (rule (compose (compose m3 m2) m1)
          (compose m3 (compose m2 m1)))))

;;
;; products
;;

;; TODO somehow store the constraint that this category has products
;; TODO require project-left & co to be defined dependently on the category
;; TODO split, project-left, project-right

(defn product [o1 o2]
  (program-with-symbols [o1 o2 :local category]
    ((object :category category) o1)
    ((object :category category) o2)
    ;; TODO verify that product exists
    ;; TODO initiality constraint
    ;; TODO 2-category embeddings
    (object :category category :value [::product o1 o2])))

(defn project-left []
  (program-with-symbols [:local o1 o2]
    (morphism :source (product o1 o2)
              :target o1
              :value ::project-left)))

(defn project-right []
  (program-with-symbols [:local o1 o2]
    (morphism :source (product o1 o2)
              :target o2
              :value ::project-right)))

(defn split [f g]
  (program-with-symbols [f g :local a b c]
    ((morphism :source c :target a) f)
    ((morphism :source c :target b) g)
    (morphism :source c :target (product a b) :value [::split f g])))

(defn product-cancellation-left-rule []
  (program-with-symbols [:local f g]
    (rule (compose (project-left) (split f g))
          f)))

(defn product-cancellation-right-rule []
  (program-with-symbols [:local f g]
    (rule (compose (project-right) (split f g))
          g)))

;; TODO overload 'product'?
(defn morphism-product [f g]
  (program-with-symbols [f g]
    (split (compose f (project-left))
           (compose g (project-right)))))

;;
;; sums
;;

(defn sum [o1 o2]
  (program-with-symbols [o1 o2 :local category]
    ((object :category category) o1)
    ((object :category category) o2)
    (object :category category :value [::sum o1 o2])))

(defn inject-left []
  (program-with-symbols [:local o1 o2]
    (morphism :source o1 :target (sum o1 o2) :value ::inject-left)))

(defn inject-right []
  (program-with-symbols [:local o1 o2]
    (morphism :source o2 :target (sum o1 o2) :value ::inject-right)))

(defn junc [f g]
  (program-with-symbols [f g :local a b c]
    ((morphism :source a :target c) f)
    ((morphism :source b :target c) g)
    (morphism :source (sum a b) :target c :value [::junc f g])))

(defn sum-cancellation-left-rule []
  (program-with-symbols [:local f g]
    (rule (compose (junc f g) (inject-left))
          f)))

(defn sum-cancellation-right-rule []
  (program-with-symbols [:local f g]
    (rule (compose (junc f g) (inject-right))
          g)))

;; TODO overload 'sum'?
(defn morphism-sum [f g]
  (program-with-symbols [f g])
  (junc (compose (inject-left) f)
        (compose (inject-right) g)))

;; TODO distributivity (via bi-functoriality?)

;;
;; monads
;;

;; TODO

;;
;; unsorted
;;

;; TODO functor
;; TODO bi-functor
;; TODO initiality

;;
;; scratchpad for term rewriting
;;

;; TODO trampoline
;; (defn simplify [form rules]
;;   (match (compose m1 m2)
;;     (apply-rules rules (compose (simplify m1) (simplify m2)))))

(comment
  (require '[clojure.pprint :refer [pprint]])

  (pprint
   (macroexpand-1
    '(program-with-symbols [:local o1 o2]
       (morphism :source (product o1 o2)
                 :target o1
                 :value ::project-left))))
  
  
  (pprint ((sum-cancellation-left-rule) 'ret))
  (pprint (run-program (sum (object :value :A) (object :value :B))))
  (pprint (run-program (inject-left)))
  (pprint (run-program (sum-cancellation-left-rule)))

  (pprint (run-program (compose (morphism :category :set
                                          :source (object :catecory :set :value :a)
                                          :target (object :category :set :value :b))
                                (identity))))

  (require '[clojure.walk :as walk])

  (let [rules [(left-identity-rule)
               (right-identity-rule)]
        ;; TODO can a rule restrict the validity of a logic program?
        ;; - e.g. applying rule 'f idempotent' on '(* f f)' should not return anything
        ;; TODO can tree be a logic program too? then it must apply to ALL solutions!
        tree (first (run-program (compose (morphism :category :set
                                                    :source (object :value :a)
                                                    :target (object :value :b)
                                                    :value :f)
                                          (identity))))]
    (query-rewriter.rewrite/apply-rules rules tree))
  ;; output is mainly:
  ;;
  ;; (first (run-program (morphism :category :set
  ;;                               :source (object :value :a)
  ;;                               :target (object :value :b)
  ;;                               :value :f))

  
  ;; )



;; TODO make dir-local: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; Local Variables:
;; eval: (put-clojure-indent 'program-with-symbols 1)
;; End:
