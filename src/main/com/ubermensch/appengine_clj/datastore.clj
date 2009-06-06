; Originally ported from appengine-clj.datastore at
; http://github.com/duelinmarkers/appengine-clj/tree/master 
;
; Copyright© 2009 John D. Hume.
;
; Released under an MIT license.
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the “Software”),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.

(ns com.ubermensch.appengine-clj.datastore
  (:import [com.google.appengine.api.datastore DatastoreServiceFactory
                                               Entity
                                               Key
                                               Query])
  (:use [clojure.contrib.test-is :only (with-test is)])
  (:refer-clojure :exclude [get]))

(declare get)

(with-test
  (defn- empty-coll [f]
    (empty ((resolve (symbol f)) nil)))

  (is (= () (empty-coll "list")))
  (is (= [] (empty-coll "vec")))
  (is (= #{} (empty-coll "set"))))

(with-test
  (defn- reconstitute-coll [v]
    (if (instance? java.util.Collection v)
      (let [f (first v)
            r (drop 1 v)]
        (if (#{"set" "vec" "list"} f)
          (into (empty-coll f) r)
          (into [] v)))
      v))

  (is (= "test" (reconstitute-coll "test")))
  (is (= ["test"] (reconstitute-coll '("test"))))
  (is (= '("test") (reconstitute-coll ["list" "test"])))
  (is (= ["test"] (reconstitute-coll '("vec" "test"))))
  (is (= #{"test"} (reconstitute-coll ["set" "test"]))))

(defn- fetch-keys [v]
  (if (instance? java.util.Collection v)
    (when (isa? Key (type (first v))) (into (empty v) (get v)))
    v))

(defn- collection-of-entities? [a] (isa? (type a) java.util.HashMap))

(with-test
  (defn- create-value
    [v]
    (fetch-keys (reconstitute-coll v))))

(defn entity-to-map
  "Converts an instance of com.google.appengine.api.datastore.Entity
  to a PersistentHashMap with properties stored under keyword keys,
  plus the entity's kind stored under ::kind and key stored under ::key."
  [arg]
  (if (collection-of-entities? arg)
    (map entity-to-map (vals arg))
    (let [#^Entity entity arg]
      (reduce #(assoc %1 (keyword (key %2)) (create-value (val %2)))
              {::kind (.getKind entity)
               ::key (.getKey entity)
               ::entity entity}
              (.entrySet (.getProperties entity))))))

(with-test
  (defn storable-type
    "Returns a string representation of the type of the given collection for
    use in reconstituting the data later."
    [c]
    (let [t (type c)]
      (cond
        (isa? t clojure.lang.IPersistentList) "list"
        (isa? t clojure.lang.IPersistentVector) "vec"
        (isa? t clojure.lang.IPersistentSet) "set"
        :default (with-out-str (pr t)))))

  (is (= "list" (storable-type '(1 2 3))))
  (is (= "vec" (storable-type [1 2 3])))
  (is (= "set" (storable-type #{1 2 3})))
  (is (= "java.lang.String" (storable-type "test"))))

(with-test
  (defn storable-value
    "If the value is a map with a ::kind entry, it's ::key is returned
    otherwise it is returned untouched."
    [v]
    (if (and (coll? v) (not (map? v)))
      (into [] (concat [(storable-type v)] (map storable-value v)))
      (if (::kind v) (::key v) v)))

  (is (= "key" (storable-value {::kind "kind" ::key "key"})))
  (is (= ["vec" "key1" "key2"] (storable-value [{::kind "k" ::key "key1"}
                                                {::kind "k" ::key "key2"}])))
  (is (= ["list" "key1" "key2"] (storable-value '({::kind "k" ::key "key1"}
                                                  {::kind "k" ::key "key2"}))))
  (is (= (set ["set" "key1" "key2"]) ; making a set b/c order doesn't matter
         (set (storable-value #{{::kind "k" ::key "key1"}
                                {::kind "k" ::key "key2"}})))))

(defn get
  "Retrieves the identified entity/ies or raises EntityNotFoundException."
  [key]
  (entity-to-map (.get (DatastoreServiceFactory/getDatastoreService) key)))
 
(defn find-all
  "Executes the given com.google.appengine.api.datastore.Query
  and returns the results as a lazy sequence of items converted with
  entity-to-map."
  [#^Query query]
  (let [data-service (DatastoreServiceFactory/getDatastoreService)
        results (.asIterable (.prepare data-service query))]
    (map entity-to-map results)))
 
(defn put
  "Takes a map of keyword-value pairs and puts a new Entity in the Datastore.
  The map must include a ::kind String. Returns the saved Entity converted
  with entity-to-map (which will include the assigned ::key)."
  ([item] (put item nil))
  ([item #^Key parent-key]
   (let [kind (::kind item)
         entity (or (::entity item)
                    (if parent-key (Entity. kind parent-key) (Entity. kind)))
         properties (dissoc item ::kind)
         properties (dissoc item ::entity)]
     (doseq [[prop-name value] properties]
       (.setProperty entity (name prop-name) (storable-value value)))
     (.put (DatastoreServiceFactory/getDatastoreService) entity)
     (entity-to-map entity))))
 
(defn delete
  "Deletes the identified entities."
  [& #^Key keys]
  (.delete (DatastoreServiceFactory/getDatastoreService) keys))
