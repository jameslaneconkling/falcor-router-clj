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
     (and (nil? pattern-key) (empty? path-set)) parsed ;; successful pattern match
     (nil? pattern-key) (assoc parsed :remaining path-set) ;; successful pattern match [with remaining path-sets]
     (nil? key-set) {:unmatched [(concat previous-path-set path-set)] :matched []} ;; failed pattern match: path-set too short
     :else (let [{matched true unmatched false} (test-pattern pattern-key key-set)]
             (cond
               (= 0 (count matched)) {:unmatched [(concat previous-path-set path-set)] :matched []} ;; failed pattern match: key-set doesn't match pattern
               (= 0 (count unmatched)) (recur rest-pattern
                                              rest-path-set
                                              (conj previous-path-set key-set)
                                              (update parsed :matched conj matched)) ;; successful key-set match
               :else (recur rest-pattern
                            rest-path-set
                            (conj previous-path-set key-set)
                            (-> parsed
                                (update :unmatched conj (concat previous-path-set unmatched rest-path-set))
                                (update :matched conj matched)))))))) ;; successful key-set match and failed key-set match

(defn test
  [actual expected]
  (if-not (= actual expected)
    (throw (AssertionError. (str "Expected:\t" expected "\n\nActual:\t" actual)))))

(let [pattern [(partial = "resource") key? key? range?]]
  ;; test match
  (test (match-path-set pattern
                        ["resource" ["one" "two"] "label" 0])
        {:unmatched []
         :matched [["resource"] ["one" "two"] ["label"] [0]]})
  ;; test unmatched
  (test (match-path-set pattern
                        ["resource" {:to 10} "label" ["abc" {:to 1}]])
        {:unmatched [["resource" {:to 10} "label" ["abc" {:to 1}]]]
         :matched []})
  ;; test match and unmatched
  ;; TODO - BUG: intersecting path bt/ two unmatched path-sets: ["resource" {:to 10} "label" "abc"] appears in both
  ;; this path would be dispatched twice against future matching paths
  (test (match-path-set pattern
                        ["resource" ["one" {:to 10}] "label" ["abc" {:to 1}]])
        {:unmatched [["resource" {:to 10} "label" ["abc" {:to 1}]]
                     ["resource" ["one" {:to 10}] "label" "abc"]]
         :matched [["resource"] ["one"] ["label"] [{:to 1}]]})
  ;; test remaining
  (test (match-path-set pattern
                        ["resource" "one" "relation" 0 "label" 0])
        {:unmatched []
         :matched [["resource"] ["one"] ["relation"] [0]]
         :remaining ["label" 0]}))

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

(let [pattern
      [(partial = "resource") key? key? range?]]
  ;; test matched
  (test (match-path-sets pattern
                         [["resource" ["one" "two"] "label" 0]
                          ["resource" "three" "relation" {:to 4}]])
        {:unmatched []
         :matched [{:paths [["resource"] ["three"] ["relation"] [{:to 4}]]
                    :remaining nil}
                   {:paths [["resource"] ["one" "two"] ["label"] [0]]
                    :remaining nil}]})
  ;; test unmatched
  (test (match-path-sets pattern
                         [["resource" {:to 10} "label" [0 {:to 1}]]
                          ["resource" ["one" {:to 10}] "relation" ["abc" {:to 1}]]])
        {:unmatched [["resource" {:to 10} "label" [0 {:to 1}]]
                     ["resource" {:to 10} "relation" ["abc" {:to 1}]]
                     ["resource" ["one" {:to 10}] "relation" "abc"]]
         :matched [{:paths [["resource"] ["one"] ["relation"] [{:to 1}]]
                    :remaining nil}]}))
