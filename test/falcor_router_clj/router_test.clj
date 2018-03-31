(ns falcor-router-clj.router-test
  (:require [falcor-router-clj.router :refer [key?
                                              range?
                                              literal?
                                              match-path-set
                                              match-path-sets
                                              router]]
            [falcor-router-clj.range :refer [range->list]]
            [clojure.test :as t]))


(defn is-equal
  [actual expected]
  (t/is (= actual expected)))


(t/deftest falcor-router
  (t/testing "match path set"
    (let [pattern [(literal? "resource") key? key? range?]]

      ;; test match
      (is-equal (match-path-set pattern
                                ["resource" ["one" "two"] "label" 0])
                {:unmatched []
                 :matched [{:paths [["resource"] ["one" "two"] ["label"] [0]]
                            :remaining []}]})

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
                 :matched [{:paths [["resource"] ["one"] ["label"] [{:to 1}]]
                            :remaining []}]})

      ;; test remaining
      (is-equal (match-path-set pattern
                                ["resource" "one" "relation" 0 "label" 0])
                {:unmatched []
                 :matched [{:paths [["resource"] ["one"] ["relation"] [0]]
                            :remaining ["label" 0]}]})))


  (t/testing "match path sets"
    (let [pattern [(literal? "resource") key? key? range?]]

      ;; test matched
      (is-equal (match-path-sets pattern
                                 [["resource" ["one" "two"] "label" 0]
                                  ["resource" "three" "relation" {:to 4}]])
                {:unmatched []
                 :matched [{:paths [["resource"] ["one" "two"] ["label"] [0]]
                            :remaining []}
                           {:paths [["resource"] ["three"] ["relation"] [{:to 4}]]
                            :remaining []}]})

      ;; test unmatched
      (is-equal (match-path-sets pattern
                                 [["resource" {:to 10} "label" [0 {:to 1}]]
                                  ["resource" ["one" {:to 10}] "relation" ["abc" {:to 1}]]
                                  ["resource" "two" "relation" 0 "label" 0]])
                {:unmatched [["resource" {:to 10} "label" [0 {:to 1}]]
                             ["resource" {:to 10} "relation" ["abc" {:to 1}]]
                             ["resource" ["one" {:to 10}] "relation" "abc"]]
                 :matched [{:paths [["resource"] ["one"] ["relation"] [{:to 1}]]
                            :remaining []}
                           {:paths [["resource"] ["two"] ["relation"] [0]]
                            :remaining ["label" 0]}]})))

  (t/testing "router"
    (let [get-resources (fn [[_ ids predicates ranges]]
                          (for [id ids
                                predicate predicates
                                index (mapcat range->list ranges)]
                            {:path ["resource" id predicate index]
                             :value (str predicate index)}))
          get-search (fn [[_ queries search-ranges predicates predicate-ranges]]
                       (for [query queries
                             search-index (mapcat range->list search-ranges)
                             predicate predicates
                             predicate-index (mapcat range->list predicate-ranges)]
                         {:path ["search" query search-index predicate predicate-index]
                          :value (str query search-index predicate predicate-index)}))
          routes [{:pattern [(literal? "resource") key? key? range?] :handler get-resources}
                  {:pattern [(literal? "search") key? range? key? range?] :handler get-search}
                  {:pattern [(literal? "search") key? range?] :handler get-search}]
          path-sets [["resource" ["one" "two"] "label" 0]
                     ["search" "QUERY" {:from 2 :to 3} ["label" "age"] 0]]]
      (is-equal ((router routes) path-sets)
                {:unmatched [],
                 :matched [{:paths [["resource"] ["one" "two"] ["label"] [0]],
                            :remaining [],
                            :query [{:path ["resource" "one" "label" 0], :value "label0"}
                                    {:path ["resource" "two" "label" 0], :value "label0"}]}
                           {:paths [["search"] ["QUERY"] [{:from 2 :to 3}] ["label" "age"] [0]],
                            :remaining [],
                            :query [{:path ["search" "QUERY" 2 "label" 0], :value "QUERY2label0"}
                                    {:path ["search" "QUERY" 2 "age" 0], :value "QUERY2age0"}
                                    {:path ["search" "QUERY" 3 "label" 0], :value "QUERY3label0"}
                                    {:path ["search" "QUERY" 3 "age" 0], :value "QUERY3age0"}]}]}))))
