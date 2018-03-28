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


;; TODO - this could be rewritten as a reduction over the pattern
(defn match-path-set
  ([pattern path-set] (match-path-set pattern path-set [] {:unmatched [] :matched []}))
  ([[pattern-key & rest-pattern]
    [key-set & rest-path-set :as path-set]
    previous-path-set
    parsed]
   (cond
     ;; successful pattern match
     (and (nil? pattern-key) (empty? path-set)) parsed
     ;; successful pattern match [with remaining path-sets]
     (nil? pattern-key) (assoc parsed :remaining path-set)
     ;; failed pattern match: path-set too short
     (nil? key-set) {:unmatched [(concat previous-path-set path-set)] :matched []}
     :else (let [{matched true unmatched false} (test-pattern pattern-key key-set)]
             (if (empty? matched)
               ;; failed pattern match: key-set doesn't match pattern
               {:unmatched [(concat previous-path-set path-set)] :matched []}
               ;; successful key-set match
               (recur rest-pattern
                      rest-path-set
                      (conj previous-path-set key-set)
                      (cond-> parsed
                        (not (empty? unmatched)) (update :unmatched conj (concat previous-path-set
                                                                                 unmatched
                                                                                 rest-path-set))
                        true (update :matched conj matched))))))))


;; (defn test
;;   [actual expected]
;;   (if-not (= actual expected)
;;     (throw (AssertionError. (str "Expected:\t" expected "\n\nActual:\t" actual)))))


(defn merge-parsed
  [parsed-path {:keys [matched unmatched remaining]}]
  (let [new-match {:paths matched :remaining remaining}]
    (cond-> parsed-path
      true (update :unmatched concat unmatched)
      (not (empty? matched)) (update :matched conj new-match))))


(defn match-path-sets
  ([pattern path-sets] (match-path-sets pattern
                                        path-sets
                                        {}))
  ([pattern [path-set & path-sets] parsed]
   (let [new-parsed (merge-parsed parsed (match-path-set pattern path-set))]
     (if (empty? path-sets)
       new-parsed
       (recur pattern path-sets new-parsed)))))
