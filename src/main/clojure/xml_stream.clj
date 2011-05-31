(ns clojure.xml-stream
  (:import
    (javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamConstants)))

(set! *warn-on-reflection* true)

(defn- stream-transformer [^XMLStreamReader stream-reader handler]
  (let [xml-path (atom [])]
    (fn [current-item item-depth]
      (if (= (.getEventType stream-reader) XMLStreamConstants/START_ELEMENT)
        (swap! xml-path #(cons (.getLocalName stream-reader) %)))
      (let [item (or (handler stream-reader current-item) current-item)
            event-type (.getEventType stream-reader)
            new-item-depth (or item-depth (if item (count @xml-path)))]
        (if (= event-type XMLStreamConstants/END_ELEMENT)
          (swap! xml-path rest))

        (cond
          (empty? @xml-path)
          item

          (and item
            (= event-type XMLStreamConstants/END_ELEMENT)
            (< (count @xml-path) new-item-depth))
          (do
            (.next stream-reader)
            item)

          true
          (do
            (.next stream-reader)
            (recur item new-item-depth)))))))

(defn- dispatch-stream [^XMLStreamReader stream-reader handler]
  (let [transformer (stream-transformer stream-reader handler)]
    (while (not= (.getEventType stream-reader) XMLStreamConstants/START_ELEMENT)
      (.next stream-reader))
    (take-while identity (repeatedly #(transformer nil nil)))))

(defn make-stream-reader
  "Create a XMLStreamReader instance from Reader or InputStream"
  [input]
  (.createXMLStreamReader (XMLInputFactory/newInstance) input))

(defn parse-dispatch
  "Start a lazy sequence of objects from the XML input"
  [input handler]
  (with-open [^XMLStreamReader stream-reader (make-stream-reader input)]
    (dispatch-stream stream-reader handler)))

(defn dispatch-partial
  "Eagerly parse a XML subtree into a sequence of objects"
  [stream-reader handler]
  (doall (dispatch-stream stream-reader handler)))

(defn first-arg
  "Returns first arg, handy for dispatch"
  [arg & whatever]
  arg)

(defn element-struct-handler [callback]
  "Turn a method dispatching on first-arg matching on :ElementName or
  [:ParentClassName :ElementName] into a handler for parse-dispatch and
  dispatch-partial.
  Handy for transferring element-based objects into defrecords.
  See the examples."
  (fn [^XMLStreamReader stream-reader ^Object item]
    (if (= (.getEventType stream-reader) XMLStreamConstants/START_ELEMENT)
      (let [qname (keyword (.getLocalName stream-reader))]
        (if item
          (callback [(keyword (.. item getClass getSimpleName)) qname]
                          stream-reader item)
          (callback qname stream-reader))))))

(defn attribute-value [^XMLStreamReader stream-reader qname]
  "When positioned on START_ELEMENT, read attribute by name"
  (.getAttributeValue stream-reader nil qname))

(defn element-text [^XMLStreamReader stream-reader]
  "When positioned on START_ELEMENT with text inside, read that text.
  Might fail terribly because no checks are made."
  (.next stream-reader)
  (.getText stream-reader))

