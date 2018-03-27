(ns falcor-router-clj.router
  (:require [clojure.core.match :refer [match]]))

(defn key?
  [key]
  (or (string? key) (number? key)))

(defn range?
  [falcor-range]
  (match falcor-range
         { :from _ :to _ } true
         { :to _ } true
         (_ :guard integer?) true
         :else false))

(defn test-pattern
  [pattern-key key-set]
  (let [key-setv (if (sequential? key-set) key-set (vector key-set))]
    (group-by pattern-key key-setv)))


(defn match-path-set
  ([pattern path-set] (match-path-set pattern path-set [] {:unmatched [] :matched []}))
  ([[pattern-key & rest-pattern]
    [key-set & rest-path-set :as path-set]
    previous-path-set
    parsed]
   (cond
     (nil? pattern-key) (assoc parsed :remaining path-set) ;; successful pattern match
     (nil? key-set) {:unmatched [(concat previous-path-set path-set)]} ;; failed pattern match: path-set too short
     :else (let [{matched true unmatched false} (test-pattern pattern-key key-set)]
             (cond
               (= 0 (count matched)) {:unmatched [(concat previous-path-set path-set)]} ;; failed pattern match: key-set doesn't match pattern
               (= 0 (count unmatched)) (recur rest-pattern
                                              rest-path-set
                                              (conj previous-path-set key-set)
                                              (update-in parsed [:matched] conj matched)) ;; successful key-set match
               :else (recur rest-pattern
                            rest-path-set
                            (conj previous-path-set key-set)
                            (-> parsed
                                (update-in [:unmatched] conj (concat previous-path-set unmatched rest-path-set))
                                (update-in [:matched] conj matched)))))))) ;; successful key-set match and failed key-set match


(match-path-set [keyword? (partial = "thing") keyword? keyword?] [:a ["thing" "thingg"] :b [:c "string"] :e])

(match-path-set [(partial = "thing") keyword? keyword?] [:a ["thing" "thingg"] :b [:c "string"] :e])

(defn match-path-sets
  ([pattern path-sets] (match-path-sets pattern
                                        path-sets
                                        {}))
  ([pattern [path-set & path-sets] parsed]
   (let [new-parsed (merge-with into parsed (match-path-set pattern path-set))]
     (if (empty? path-sets)
       new-parsed
       (recur pattern path-sets new-parsed)))))

(match-path-sets
 [keyword? (partial = "thing") keyword? keyword?]
 [[:a ["thing" "thingg"] :b [:c "string"] :e]
  [:a "thing" :bb :cc]])

(->> (match-path-sets
      [keyword? (partial = "thing") keyword? keyword?]
      [[:a ["thing" "thingg"] :b [:c "string"] :e]])
     :unmatched
     first)
