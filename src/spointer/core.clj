(ns spointer.core
  (:require [clojure.walk :as walk])
  (:import [java.io FileInputStream InputStreamReader]
           [clojure.lang LineNumberingPushbackReader LispReader]))

(defn read-file [filename]
  (let [in (InputStreamReader. (FileInputStream. filename))
        pushback-reader (LineNumberingPushbackReader. in)]
    (take-while #(not= % :eof)
                (repeatedly #(LispReader/read pushback-reader false :eof false)))))

(defn same-map [coll-for-type fun & colls]
  (if (list? coll-for-type)
    (apply map fun colls)
    (into (empty coll-for-type) (apply map fun colls))))

(def map-entry?
  (partial instance? java.util.Map$Entry))

(defn fix-map-entry [x]
  (if (map-entry? x)
    [(first x) (second x)] x))

(defn spointer-walk
  ([fun coll]
     (spointer-walk fun coll []))
  ([fun coll spointer]
     (same-map
      coll
      (fn [[index x]]
        (let [x (fix-map-entry x)
              p (conj spointer index)]
          (fun p (if (sequential? x) (spointer-walk fun x p) x))))
      (map vector (iterate inc 0) coll))))

(def can-have-meta?
  (partial instance? clojure.lang.IObj))

(defn add-spointers-meta [tree]
  (reverse ;;wtf
   (spointer-walk
    (fn [spointer x]
      (if (can-have-meta? x)
        (with-meta x {:spointer spointer}) x)) tree)))

(defn resolve-spointer [spointer tree]
  (reduce #(nth %1 %2) tree spointer))

;;(pprint (add-spointers-meta (read-file "src/spointer/core.clj")))
