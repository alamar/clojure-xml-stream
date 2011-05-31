(ns example
  (:import (java.io FileInputStream))
  (:use clojure.xml-stream))

(defrecord TreeSpecies [id name])
(defrecord Forest [id trees name])
(defrecord Tree [species-id branches])

(defmulti ground-element first-arg)

(defmulti tree-element first-arg)

(defmethod ground-element :tree [_ stream-reader]
  (TreeSpecies. (attribute-value stream-reader "id") nil))

(defmethod ground-element [:TreeSpecies :name] [_ stream-reader tree]
  (assoc tree :name (element-text stream-reader)))

(defmethod ground-element :forest [_ stream-reader]
  (Forest. (attribute-value stream-reader "id") [] nil))

(defmethod ground-element [:Forest :name] [_ stream-reader forest]
  (assoc forest :name (element-text stream-reader)))

(defmethod ground-element [:Forest :tree] [_ stream-reader forest]
  (assoc forest :trees
    (conj (:trees forest)
      (Tree. (attribute-value stream-reader "refid")
        (dispatch-partial stream-reader
          (element-struct-handler tree-element))))))

(defmethod tree-element :branch [_ stream-reader]
  (keyword (attribute-value stream-reader "direction")))

(defmethod ground-element :default [token & whatever]
  (comment println token))
(defmethod tree-element :default [token & whatever]
  (comment println token))

(defn run [path]
  (with-open [input-stream (FileInputStream. path)]
    (let [handler (element-struct-handler ground-element)
          objects (parse-dispatch input-stream handler)]
      (doseq [object objects] (println object)))))

