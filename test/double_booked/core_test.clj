(ns double-booked.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [double-booked.core :as core]))

(def test-events [{:id :evt1 :from 1 :to 2}
                  {:id :evt2 :from 10 :to 12}
                  {:id :evt3 :from 1.3 :to 2}
                  {:id :evt4 :from 2 :to 3}
                  {:id :evt5 :from 2 :to 2.2}
                  {:id :evt6 :from 2 :to 2.3}])

(deftest test-type=from?
  (is (true? (core/type=from? {:type :from}
                              {:type :from}
                              {:type :from})))
  (is (false? (core/type=from? {:type :from}
                               {:type :o}
                               {:type :from}))))

(deftest test-from?
  (is (true? (core/from? {:type :from})))
  (is (false? (core/from? {}))))

(deftest test-events->timeline
  (is (= (core/events->timeline test-events)
         [{:type :from :date 1 :id :evt1}
          {:type :from :date 1.3 :id :evt3}
          {:type :to :date 2 :id :evt1}
          {:type :to :date 2 :id :evt3}
          {:type :from :date 2 :id :evt4}
          {:type :from :date 2 :id :evt5}
          {:type :from :date 2 :id :evt6}
          {:type :to :date 2.2 :id :evt5}
          {:type :to :date 2.3 :id :evt6}
          {:type :to :date 3 :id :evt4}
          {:type :from :date 10 :id :evt2}
          {:type :to :date 12 :id :evt2}])))

(deftest test-previous-matching
  (is (= (core/previous-matching [{:type :to :id 1}
                                  {:type :from :id 2}
                                  {:type :to :id 3}
                                  {:type :from :id 4}
                                  {:type :from :id 5}]
                                 #(= (:type %) :from))
         [{:type :from :id 5}
          {:type :from :id 4}])))

(deftest test-build-pairs
  (is (= (core/build-pairs [{:type :to :id :e1}
                            {:type :from :id :e2}
                            {:type :to :id :e3}
                            {:type :from :id :e4}
                            {:type :from :id :e5}]
                           {:id :e6})
         [[:e6 :e5]
          [:e6 :e4]])))

(deftest test-index-coll
  (let [coll [{:id 1 :name "a"}
              {:id 2 :name "b"}
              {:id 3 :name "c"}]]
    (is (= (core/index-coll coll :id)
           {1 {:id 1 :name "a"}
            2 {:id 2 :name "b"}
            3 {:id 3 :name "c"}}))
    (is (= (core/index-coll coll :name)
           {"a" {:id 1 :name "a"}
            "b" {:id 2 :name "b"}
            "c" {:id 3 :name "c"}}))))

(deftest test-events->point-pairs
  (is (= (core/events->point-pairs test-events)
         #{[:evt5 :evt4]
           [:evt3 :evt1]
           [:evt6 :evt4]
           [:evt6 :evt5]})))

(deftest test-events->overlapping-pairs
  (is (= (set (core/events->overlapping-pairs test-events))
         #{[{:id :evt5 :from 2 :to 2.2} {:id :evt4 :from 2 :to 3}]
           [{:id :evt3 :from 1.3 :to 2} {:id :evt1 :from 1 :to 2}]
           [{:id :evt6 :from 2 :to 2.3} {:id :evt4 :from 2 :to 3}]
           [{:id :evt6 :from 2 :to 2.3} {:id :evt5 :from 2 :to 2.2}]})))
