(ns demo.AntsAppletRunner
  (:import (javax.swing JFrame))
  (:gen-class
   :main -main))

(compile 'demo.AntsApplet2)

(defn -main []
  (let [applet (new demo.AntsApplet2)] ;; new AntsApplet2 object, as applet
    (doto (JFrame. "Ants Simulation")  ;; applet.JFrame("Ants Simulation").add().pack()...
      (.add (.getContentPane applet))
      (.pack)
      (.setLocationByPlatform true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))
