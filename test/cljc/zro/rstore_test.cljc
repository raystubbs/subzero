(ns zro.rstore-test
  (:require
   [zro.rstore :as rstore]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer-macros [deftest is]])))

(deftest affected-paths-test
  (let [!rstore (rstore/rstore {:x {:y {:z 1}} :m {}})
        !affected-paths (atom #{})]
    (rstore/watch !rstore ::xyz-path [:x :y :z]
      (fn [& _] (swap! !affected-paths conj [:x :y :z])))
    (rstore/watch !rstore ::xy-path [:x :y]
      (fn [& _] (swap! !affected-paths conj [:x :y])))
    (rstore/watch !rstore ::x-path [:x]
      (fn [& _] (swap! !affected-paths conj [:x])))
    (rstore/watch !rstore ::root-path []
      (fn [& _] (swap! !affected-paths conj []))) 
    (rstore/watch !rstore ::m-path [:m]
      (fn [& _] (swap! !affected-paths conj [:m])))
    
    (rstore/patch! !rstore
      {:path [:x :y]
       :change [:value 2]})
    
    (is (= @!affected-paths #{[] [:x] [:x :y] [:x :y :z]}))))

(deftest assoc-op-test
  (let [orig {:foo "foo"}
        !rstore (rstore/rstore orig)
        [old new] (rstore/patch! !rstore {:path [] :change [:assoc :foo "FOO" :bar "BAR"]})]
    (is (= old orig))
    (is (= new {:foo "FOO" :bar "BAR"}))))

(deftest clear-op-test
  (let [orig {:foo "foo" :x [1 2]}
        !rstore (rstore/rstore orig)
        [old new] (rstore/patch! !rstore
                    [{:path [] :change [:clear :foo]}
                     {:path [:x] :change [:clear 1]}])]
    (is (= old orig))
    (is (= new {:x [1]}))))

(deftest conj-op-test
  (let [orig {:x [1]}
        !rstore (rstore/rstore orig)
        [old new] (rstore/patch! !rstore [{:path [:x] :change [:conj 2 3]}])]
    (is (= old orig))
    (is (= new {:x [1 2 3]}))))

(deftest into-op-test
  (let [orig {:x [1]}
        !rstore (rstore/rstore orig)
        [old new] (rstore/patch! !rstore [{:path [:x] :change [:into [2 3]]}])]
    (is (= old orig))
    (is (= new {:x [1 2 3]}))))

(deftest call-op-test
  (let [orig {:x 1}
        !rstore (rstore/rstore orig)
        [old new] (rstore/patch! !rstore [{:path [:x] :change [:call inc]}])]
    (is (= old orig))
    (is (= new {:x 2}))))