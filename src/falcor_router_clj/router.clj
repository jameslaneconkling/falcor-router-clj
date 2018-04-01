(ns falcor-router-clj.router
  (:require [clojure.core.match :refer [match]]
            [falcor-router-clj.range :refer [range->list]]))


(defn is-equal
  [actual expected]
  (if-not (= actual expected)
    (throw (AssertionError. (str "Expected:\t" expected "\n\nActual:\t" actual)))))


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


(defn literal?
  [literal]
  (partial = literal))


(defn test-pattern
  [pattern-key key-set]
  (let [key-setv (if (sequential? key-set) key-set (vector key-set))]
    (group-by pattern-key key-setv)))


;; TODO - could this be rewritten as a reduction over the pattern
(defn match-path-set
  ([pattern path-set]
   (if (< (count path-set) (count pattern))
     ;; failed pattern match: path-set too short
     {:unmatched [path-set] :matched []}
     (loop [[pattern-key & rest-pattern] pattern
            [key-set & rest-path-set :as remaining-path-set] path-set
            previous-path-set []
            parsed {:unmatched [] :matched [{:path-set []}]}]
       (if (nil? pattern-key)
         ;; successful pattern match
         (assoc-in parsed [:matched 0 :remaining] (or remaining-path-set [])) ;; TODO - destructure remaining-path-set using :or
         (let [{matched true unmatched false} (test-pattern pattern-key key-set)]
           (cond
             ;; failed pattern match: key-set doesn't match pattern
             (empty? matched) {:unmatched [path-set] :matched []}
             ;; successful key-set match
             :else (recur rest-pattern
                          rest-path-set
                          (conj previous-path-set key-set)
                          (cond-> parsed
                            (not (empty? unmatched)) (update :unmatched
                                                             conj
                                                             (concat previous-path-set
                                                                     unmatched
                                                                     rest-path-set))
                            true (update-in [:matched 0 :path-set] conj matched))))))))))


(defn match-path-sets
  [pattern path-sets]
  (reduce #(merge-with concat %1 (match-path-set pattern %2))
          {}
          path-sets))


(defn query-route
  [{:keys [pattern handler]}
   path-sets]
  (update (match-path-sets pattern path-sets)
          :matched
          (partial map #(assoc % :query (handler (:path-set %))))))


(defn merge-parsed
  [parsed {:keys [unmatched matched]}]
  (-> parsed
      (assoc :unmatched unmatched)
      (update :matched concat matched)))


(defn router
  [routes]
  (fn [path-sets]
    (reduce (fn [parsed route]
              (let [next-parsed (merge-parsed parsed
                                              (query-route route (:unmatched parsed)))]
                (if (empty? (:unmatched next-parsed))
                  (reduced next-parsed)
                  next-parsed)))
            {:unmatched path-sets :matched []}
            routes)))


(defn path-values->json-graph
  [path-values]
  {:jsonGraph (reduce #(assoc-in %1 (:path %2) (:value %2)) {} path-values)})


;; TODO - async via core.async
;;      - combine path-value results into graphJSON
;;      - follow refs
;;      - handle unmatched routes
;;      - match data structure across calls
;;      - use spec to document/test types: path, path-set, key-set, pattern, parsed-key-set
