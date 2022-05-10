(ns demo.AntsAppletRunner
  (:import (javax.swing JFrame))
  (:gen-class
   :main -main))

(compile 'demo.AntsApplet2)

(defn -main []
  (let [applet (new demo.AntsApplet2)]
    (doto (JFrame. "Ants Simulation")
      (.add (.getContentPane applet))
      (.pack)
      (.setLocationByPlatform true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))
