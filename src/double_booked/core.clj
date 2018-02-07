(ns double-booked.core)

(defn type=from?
  [& items]
  (apply = (conj (map :type items) :from)))

(defn from?
  [{:keys [type]}]
  (= type :from))

(defn events->timeline
  "Given events returns a sorted collection of event's start/end points."
  [events]
  (->> events
       (mapcat
        (fn [{:keys [id from to]}]
          [{:type :from :date from :id id}
           {:type :to :date to :id id}]))
       (sort-by :date)))

(defn previous-matching
  "Returns previous, parallel items matching the `pred` from the `coll` starting
  from the last one."
  [coll pred]
  (take-while pred (reverse coll)))

(defn build-pairs
  "Given a collection of events and a timeline point builds pairs (represented
  as a vector of two ids) of that point and all previous, parallel event
  starting points."
  [coll {point-id :id}]
  (let [prev-froms (previous-matching coll from?)]
    (map
     (fn [{:keys [id]}]
       [point-id id])
     prev-froms)))

(defn events->point-pairs
  "Given a collection of events, returns the overllaping ones as pairs
  represented by vector of two event ids."
  [events]
  (loop [[p1 & more] (events->timeline events)
         past []
         overlapping #{}]
    (let [p2 (first more)]
      (if-not (or p1 p2)
        overlapping
        (recur more
               (conj past p1 p2)
               (if (type=from? p1 p2)
                 (into overlapping (build-pairs (conj past p1) p2))
                 overlapping))))))

(defn ^:private inflate-pair
  [db [id1 id2]]
  [(get db id1) (get db id2)])

(defn index-coll
  "Given a collection, builds a map with specified index as the key for each item."
  [coll idx]
  (reduce
   (fn [m item]
     (assoc m (get item idx) item))
   {}
   coll))

(defn events->overlapping-pairs
  "Given a collection of events, returns the overlapping ones as vector pairs."
  [events]
  (let [pairs (events->point-pairs events)
        db (index-coll events :id)]
    (map (partial inflate-pair db) pairs)))
