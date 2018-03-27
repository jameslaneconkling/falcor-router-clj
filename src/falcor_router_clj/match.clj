(ns falcor-router-clj.match
  (:require [clojure.walk :refer [walk]]
            [clojure.core.match :refer [match]]))

;; [["resource" ["schema:Person" "schema:birthPlace" "skos:prefLabel"] "skos:prefLabel" "0"]
;;  ["collection" "search:desc" "length"]
;;  ["collection" "search:desc" 0 ["schema:birthPlace" "skos:prefLabel"] "length"]
;;  ["collection" "search:desc" 0 ["schema:birthPlace" "skos:prefLabel"] 0 "uri"]
;;  ["collection" "search:desc" 0 ["schema:birthPlace" "skos:prefLabel"] 0 "skos:prefLabel" "0"]

(defn search
  [{:keys [searches search-ranges predicates predicate-ranges]}]
  (run-search ...))
(defn get-resource
  [{:keys [uris predicates predicate-ranges]}]
  (resources ...))

(falcor-router '(["collection"
                  (:key searches)
                  (:or (:range search-ranges) "length")] (:get search)
                 ["collection"
                  (:key searches)
                  (:or (:range search-ranges) "length")
                  (:optional (:key predicates)
                             (:range predicate-ranges))] {:get search}
                 ["resource"
                  (:key uris)
                  (:key predicates)
                  (:range predicate-ranges)
                  (:optional (:key d2-predicates)
                             (:range d2-predicate-ranges))] {:get get-resource
                                                             :set set-resource}))


(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [item (first colls)
          remaining (cartesian-product (rest colls))]
      (cons item remaining))))

(defn path-set->paths
  [path-set]
  (walk #(if (sequential? %) % [%]) cartesian-product path-set))

(def path-sets->paths (partial mapcat path-set->paths))


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

(defn ->range
  [from to]
  (hash-map :from from :to to))

(defn match-path
  [path]
  (match path
         (["resource"
           (uri :guard key?)
           (predicate :guard key?)
           (predicate-range :guard range?) & r] :seq) [:resource-route [uri predicate] r]
         (["collection"
           (search-desc :guard key?)
           (search-range :guard range?) & r] :seq) [:search-route [search-desc search-range] r]
         :else :error))

(defn match-path-sets
  [path-sets]
  (->> path-sets
       (mapcat path-set->paths)
       (map match-path)))


(match-path-sets
 [["resource" "abc-id" "schema:age" { :from 20 :to 30 }]])

(match-path-sets
 [["resource" "abc-id" ["label" "birthPlace"] [{ :to 5 } 10 { :from 20 :to 30 }] ["label" "uri"]]
  ["collection" "search-query" {:to 10} ["label" "birthPlace"] ["label" "uri"]]])

(defn ensure-key-sets-are-vectors
  [path-set]
  (map #(if (sequential? %) % (vector %)) path-set))

(defn key-set-contains?
  [pred]
  (partial some pred))

(defn match-path-set
  [path-set]
  (-> path-set
      ensure-key-sets-are-vectors
      (match ([(_ :guard (key-set-contains? #{"resource"}))
               (uri :guard (key-set-contains? key?))
               (predicate :guard (key-set-contains? key?))
               (predicate-range :guard (key-set-contains? range?))
               & r] :seq) [:resource-route {:uri (filter key? uri) :predicate (filter key? predicate) :predicate-range (filter range? predicate-range)} r]
             ([(_ :guard (key-set-contains? #{"search"}))
               (search-range :guard (key-set-contains? range?))] :seq)
             :else :error)))

;; successfully matches ["resource" "abc-id" "label" 0]
;; but still needs to try to match
;; ["resource" {:to 10} "label" 0]
;; ["search" ["abc-id" {:to 10}] "label" 0]
(match-path-set [["resource" "search"] ["abc-id" {:to 10}] "label" 0])
