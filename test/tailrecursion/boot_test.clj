(ns tailrecursion.boot-test
  (:require 
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]))

(defn main [boot]
  (pprint boot)
  (fn [continue]
    (fn [event]
      (continue event))))
