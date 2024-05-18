(ns subzero.equiv-proxy-test
  (:require
   [subzero.core :refer [with-const-eq with-ident-eq]]
   [cljs.test :refer-macros [deftest is]]))

(deftest ident-proxy-test
  (let [m1 (with-ident-eq (hash-map :foo "BAR"))
        m2 (with-ident-eq (hash-map :foo "BAR"))]
    (is (not= m1 m2))
    (is (= m1 m1))
    (is (= m2 m2))
    (is (map? m1))
    (is (map? m2))))

(deftest const-proxy-test
  (let [m1 (with-const-eq :foo {:foo "BAR"})
        m2 (with-const-eq :foo {:foo "BAR"})
        m3 (with-const-eq :bar {:foo "BAR"})
        m4 {:foo "BAR"}]
    (is (= m1 m2))
    (is (not= m1 m3))
    (is (not= m1 m4))
    (is (map? m1))
    (is (map? m2))
    (is (map? m3))))