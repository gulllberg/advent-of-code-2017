(ns advent-of-code-2017.reverse-polish-calculator
  (:require [ysera.test :refer [is=]]))

(defn reverse-polish-calculator
  {:test (fn []
           (is= (reverse-polish-calculator 10 4 3 + 2 * -)
                -4))}
  [& inputs]
  (first (reduce (fn [stack operator-or-number]
                   (if (number? operator-or-number)
                     (conj stack operator-or-number)
                     (conj (drop 2 stack) (operator-or-number (second stack) (first stack)))))
                 '()
                 inputs)))