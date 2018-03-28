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


(defn match-path-set*
  [pattern path-set]
  (reduce (fn [parsed [pattern-key key-set]]
            (cond
              ))
          {:unmatched [] :matched []}
          (map vector pattern path-set)))


;; TODO - this could be rewritten as a reduction over the pattern
(defn match-path-set
  ([pattern path-set]
   (if (< (count path-set) (count pattern))
     ;; failed pattern match: path-set too short
     {:unmatched [path-set] :matched []}
     (loop [[pattern-key & rest-pattern] pattern
            [key-set & rest-path-set :as path-set] path-set
            previous-path-set []
            parsed {:unmatched [] :matched []}]
       (cond
         ;; successful pattern match
         (and (nil? pattern-key) (empty? rest-path-set)) parsed
         ;; successful pattern match [with remaining path-sets]
         (nil? pattern-key) (assoc parsed :remaining path-set)
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
                            true (update :matched conj matched))))))))))


(defn is-equal
  [actual expected]
  (if-not (= actual expected)
    (throw (AssertionError. (str "Expected:\t" expected "\n\nActual:\t" actual)))))


(let [pattern [(partial = "resource") key? key? range?]]

  ;; test match
  (is-equal (match-path-set pattern
                            ["resource" ["one" "two"] "label" 0])
            {:unmatched []
             :matched [["resource"] ["one" "two"] ["label"] [0]]})

  ;; test unmatched - bad pattern
  (is-equal (match-path-set pattern
                            ["resource" {:to 10} "label" ["abc" {:to 1}]])
            {:unmatched [["resource" {:to 10} "label" ["abc" {:to 1}]]]
             :matched []})

  ;; test unmatched - short pathset
  (is-equal (match-path-set pattern
                            ["resource" ["one" "two"] "label"])
            {:unmatched [["resource" ["one" "two"] "label"]]
             :matched []})

  ;; test match and unmatched
  ;; TODO - BUG: intersecting path bt/ two unmatched path-sets: ["resource" {:to 10} "label" "abc"] appears in both
  ;; this path would be dispatched twice against future matching paths
  (is-equal (match-path-set pattern
                            ["resource" ["one" {:to 10}] "label" ["abc" {:to 1}]])
            {:unmatched [["resource" {:to 10} "label" ["abc" {:to 1}]]
                         ["resource" ["one" {:to 10}] "label" "abc"]]
             :matched [["resource"] ["one"] ["label"] [{:to 1}]]})

  ;; test remaining
  (is-equal (match-path-set pattern
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
