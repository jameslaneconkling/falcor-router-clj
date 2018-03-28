(ns falcor-router-clj.router-test
  (:require [falcor-router-clj.router :refer [key?
                                              range?
                                              match-path-set
                                              match-path-sets]]
            [clojure.test :as t]))

(defn is-equal
  [actual expected]
  (t/is (= actual expected)))


(t/deftest router
  (t/testing "match path set"
    (let [pattern [(partial = "resource") key? key? range?]]

      ;; test match
      (is-equal (match-path-set pattern
                                ["resource" ["one" "two"] "label" 0])
                {:unmatched []
                 :matched [["resource"] ["one" "two"] ["label"] [0]]})

      ;; test unmatched
      (is-equal (match-path-set pattern
                                ["resource" {:to 10} "label" ["abc" {:to 1}]])
                {:unmatched [["resource" {:to 10} "label" ["abc" {:to 1}]]]
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
                 :remaining ["label" 0]})))

  (t/testing "match path sets"
    (let [pattern [(partial = "resource") key? key? range?]]

      ;; test matched
      (is-equal (match-path-sets pattern
                                 [["resource" ["one" "two"] "label" 0]
                                  ["resource" "three" "relation" {:to 4}]])
                {:unmatched []
                 :matched [{:paths [["resource"] ["three"] ["relation"] [{:to 4}]]
                            :remaining nil}
                           {:paths [["resource"] ["one" "two"] ["label"] [0]]
                            :remaining nil}]})

      ;; test unmatched
      (is-equal (match-path-sets pattern
                                 [["resource" {:to 10} "label" [0 {:to 1}]]
                                  ["resource" ["one" {:to 10}] "relation" ["abc" {:to 1}]]])
                {:unmatched [["resource" {:to 10} "label" [0 {:to 1}]]
                             ["resource" {:to 10} "relation" ["abc" {:to 1}]]
                             ["resource" ["one" {:to 10}] "relation" "abc"]]
                 :matched [{:paths [["resource"] ["one"] ["relation"] [{:to 1}]]
                            :remaining nil}]}))))
