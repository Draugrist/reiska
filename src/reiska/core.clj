(ns reiska.core
  (:require
    [reiska.render :as render]))

(defn do-render [scene]
  (render/render scene (str (:name scene) ".png")))

(defn prepare-scene [scene]
  (let [lights (filter :emission (:objects scene))]
    (println (str "Found " (count lights) " lights."))
    (println (str "Total objects: " (count (:objects scene))))
    (assoc scene :lights lights)))

(defn load-scene-from-file [file]
  (println "Loading scene from" file)
  (prepare-scene (read-string (slurp file))))

(defn -main [& args]
  (if (= 1 (count args))
    (do-render (load-scene-from-file (first args)))
    (println "Usage: java -jar reiska <scene-file>")))
