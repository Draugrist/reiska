(ns reiska.rendertest
  (:require
    [clojure.test :refer :all]
    [euclidean.math.vector :as v]
    [reiska.render :as render]))

(def origo (v/vector 0 0 0))

(def scene-simple
  {:name       "scene-simple"
   :background (v/vector 1 1 1)
   :camera
   {:center    (v/vector -10 0 0)
    :fov       45
    :direction (v/vector 1 0 0)}
   :objects
   [{:center       (v/vector 0 0 0)
     :radius       1
     :color        (v/vector 1 0 0)
     :reflectivity 0
     :transparency 0}
    {:center       (v/vector 1 1 1)
     :radius       1
     :color        (v/vector 0 1 0)
     :reflectivity 0
     :transparency 0}
    {:center       (v/vector -1 -1 -1)
     :radius       1
     :color        (v/vector 0 0 1)
     :reflectivity 0
     :transparency 0}
    {:center       (v/vector 0 0 10)
     :radius       1
     :color        (v/vector 1.0 1.0 1.0)
     :reflectivity 0
     :transparency 0
     :emission     (v/vector 3 3 3)}]})

(deftest intercept
  (testing "intercept hits"
    (let [hit (render/intercept-sphere origo (v/vector 0 0 -1) {:center (v/vector 0 0 -20) :radius 2})]
      (is (= 18.0 (:point hit)))))
  (testing "intercept misses"
    (is (nil? (render/intercept-sphere origo (v/vector 0 0 1) {:center (v/vector 0 0 -20) :radius 2})))))

(deftest test-simple-scene
  (testing "simple scene"
    (testing "intercept first"
      (let [hit (render/intercept-sphere origo (v/vector 1 0 0) (-> scene-simple :objects first))]
        (is (= 5.0 (:point hit)))))
    (testing "closest-hit"
      (let [closest (render/get-closest-hit origo (v/vector 1 0 0) (:objects scene-simple))]
        (is (= 5.0 (:point closest)))))))
