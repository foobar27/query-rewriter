(ns query-rewriter.category
  (:require [query-rewriter.rewrite :as rewrite :refer [program-with-symbols rule run-program]]))

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





;; (defrule associativity-rule-1 [m1 m2 m3]
;;   (compose m3 (compose m2 m1))
;;   -> (compose (compose m3 m2) m1))

;; (defrule associativity-rule-2 [m1 m2 m3]
;;   (compose (compose m3 m2) m1)
;;   -> (compose m3 (compose m2 m1)))


;;
;; products
;;

;; TODO somehow store the constraint that this category has products
;; TODO require project-left & co to be defined dependently on the category
;; TODO split, project-left, project-right

;; (defrule product-cancellation-left-rule [f g]
;;   (compose (project-left) (split f g))
;;   -> f)

;; (defrule product-cancellation-right-rule [f g]
;;   (compose (project-right) (split f g))
;;   -> g)

;; (defpattern morphism-product [f g]
;;   (split (compose f (project-left))
;;          (compose g (project-right))))

;;
;; sums
;;

;; (defrule sum-cancellation-left-rule [f g]
;;   (compose (junc f g) (inject-left))
;;   -> f)

;; (defrule sum-cancellation-right-rule [f g]
;;   (compose (junc f g) (inject-right))
;;   -> g)

;; (defpattern morphism-sum [f g]
;;   (split (compose (inject-left) f)
;;          (compose (inject-right) g)))

;;
;; monads
;;

;; TODO

;;
;; unsorted
;;

;; [:morphism
;;  C
;;  [A B]
;;  [:compose
;;   [:morphism
;;    C
;;    [B B]
;;    'identity]
;;   [:morphism
;;    C
;;    [A B]
;;    f]]]
;; -> [:morphism C [A B] f]

;; TODO functor
;; TODO bi-functor
;; TODO initiality

(comment
  (require '[clojure.pprint :refer [pprint]])
  (pprint ((compose 'm1 'm2) 'ret))

  (pprint (run-program (left-identity-rule))))


;; TODO make dir-local: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; Local Variables:
;; eval: (put-clojure-indent 'program-with-symbols 1)
;; End:
