(ns reiska.output
  (:import (java.awt Color)
           (java.awt.image BufferedImage)
           (java.io FileOutputStream)
           (javax.imageio ImageIO)))

(defn ->col [v]
  (let [r (int (* (min 1.0 (v 0)) 255))
        g (int (* (min 1.0 (v 1)) 255))
        b (int (* (min 1.0 (v 2)) 255))]
    (.getRGB (Color. ^int r ^int g ^int b))))

(defn raw-data->img
  [raw-data]
  (let [{:keys [width height data]} raw-data
        v   (vec data)
        img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
    (doseq [y (range height) x (range width)]
      ; Flip y-axis so that images are not inverted
      (.setRGB img x (- height y 1) (->col (v (+ x (* y width))))))
    img))

(defn img->file
  [^BufferedImage img ^String filename]
  (with-open [out (FileOutputStream. filename)]
    (ImageIO/write img "png" out)
    (.flush out)))
