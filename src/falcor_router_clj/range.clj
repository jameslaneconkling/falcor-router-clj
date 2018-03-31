(ns falcor-router-clj.range)


(defprotocol IRange
  "Represents a contiguous range of integer numbers of length 1+"
  (range->list [item] "Convert range to a list of integers")
  (range->offset-limit [item] "Convert range to hashmap of offset/limit values for use in a SQL or SPARQL query"))


(extend-protocol IRange
  clojure.lang.PersistentArrayMap
  (range->list
    [{:keys [from to] :or {from 0}}]
    (range from (inc to)))
  (range->offset-limit
    [{:keys [from to] :or {from 0}}]
    (hash-map :offset from :limit (- (inc to) from)))

  java.lang.Number
  (range->list
    [number]
    (range number (inc number)))
  (range->offset-limit
    [element]
    (hash-map :offset element :limit 1)))
