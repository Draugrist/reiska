(ns reiska.render
  (:require
    [clojure.pprint :as pp]
    [euclidean.math.vector :as v]
    [reiska.output :as output]))

(def width 640)
(def height 480)
(def max-depth 5)
(def bias 1e-4)
(def default-up (v/vector 0 0 1))

(defn intercept-sphere [origin ray-dir {:keys [center radius] :as sphere}]
  (let [l   (v/sub center origin)
        tca (v/dot l ray-dir)]
    (if (>= tca 0)
      (let [d2 (- (v/dot l l) (* tca tca))
            r2 (* radius radius)]
        (if (<= d2 r2)
          (let [thc (Math/sqrt (- r2 d2))
                t0  (- tca thc)
                t1  (+ tca thc)]
            (if (< t0 0)
              {:point t1 :sphere sphere}
              {:point t0 :sphere sphere}))
          nil))
      nil)))

(defn get-closest-hit [origin ray-dir objects]
  (first (sort-by :point (filter some? (map (partial intercept-sphere origin ray-dir) objects)))))

(defn object->light [hit-object point-of-hit normal-hit objects surface-color light]
  (let [light-direction (v/normalize (v/sub (:center light) point-of-hit))
        other-objects   (remove #(= % light) objects)
        origin          (v/add point-of-hit (v/scale normal-hit bias))
        hit             (get-closest-hit origin light-direction other-objects)
        transmission    (if hit 0 1)]
    (v/add surface-color
           (v/mult (:color hit-object)
                   (v/scale
                     (:emission light)
                     (* transmission
                        (max 0.0
                             (v/dot normal-hit light-direction))))))))

(defn trace [origin ray-dir scene depth]
  (let [closest-hit (get-closest-hit origin ray-dir (:objects scene))]
    (if closest-hit
      (let [{:keys [point sphere]} closest-hit
            point-of-hit (v/add origin (v/scale ray-dir point))
            normal-hit   (v/normalize (v/sub point-of-hit (:center sphere)))
            inside       (> (v/dot ray-dir normal-hit) 0)
            normal-hit   (if inside (v/sub normal-hit) normal-hit)]
        ;TODO transparent or reflective object
        (let [surface (reduce (partial object->light sphere point-of-hit normal-hit (:objects scene)) (v/vector 0 0 0) (:lights scene))]
          (if (:emission sphere)
            (v/add (:emission sphere) surface)
            surface)))
      (:background scene))))

(defn render [scene outfile]
  (pp/pprint scene)
  (println "Printing to" outfile)
  (let [{:keys [center fov direction up]} (:camera scene)
        hw-ratio      (/ height width)
        fov-radians   (/ (* Math/PI 0.5 fov) 180.0)
        half-width    (Math/tan fov-radians)
        half-height   (* hw-ratio half-width)
        camera-width  (* 2 half-width)
        camera-height (* 2 half-height)
        pixel-width   (/ camera-width (- width 1))
        pixel-height  (/ camera-height (- height -1))
        eye-vector    (v/normalize (v/sub direction center))
        v-right       (v/normalize (v/cross eye-vector (or up default-up)))
        v-up          (v/normalize (v/cross v-right eye-vector))
        pixels        (for [y (range height) x (range width)]
                        (let [xcomp   (v/scale v-right (- (* x pixel-width) half-width))
                              ycomp   (v/scale v-up (- (* y pixel-height) half-height))
                              ray-dir (v/normalize (v/add eye-vector xcomp ycomp))]
                          (trace center ray-dir scene 0)))]
    (output/img->file
      (output/raw-data->img {:width width :height height :data pixels})
      outfile)))
