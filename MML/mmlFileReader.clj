(ns mml.mmlFileReader)

;This is a library for reading MML files and parsing the data structures.
;It relies on the libraries found in the namespace mml.mmlParse.
;Basically this works as sort of a wrapper for that library.
;This, in turn, should be used with mml.clj so that it can finalize the data structuring

(import lib.libClj)  ;A general-purpose Clojure library.
(import mml.mmlParse);The backend to this library.

(def *hopper* 0);Keeps the position in the file as to not rewrite old info.

(defn choose [functions triggers actual]
  (loop [l1 functions, l2 triggers]
    (cond
      (or (bad? l1) (bad? l2)) fake
      (= (first l2) actual) (first l1))
      :else (recur (rest l1)
                   (rest l2))))

(def *global-objects* '())


(defn make-object [sets gobjlist];PLEASE PELASE PLEASE MAKE THIS ABOUT A ONE LINER
  "Make an object from a set in the file."
  (cond
    (.contains (first sets) "implicit") (make-impl-obj (after (first sets) "name=" " ") (lines-beetween "none" 0 "{" "}" (rest sets)))
    
    (.contains (first sets) "explicit") (make-expl-obj (after (first sets) "name=" " ") (lines-between "none" 0 "{" "}" (rest sets)))

    (.contains (first sets) "instantiation") (make-instantiation (after (first sets) "name=" " ") (fsoc gobjs (after (first sets) "from=" ".")))

    (.contains (first sets) "template") (make-template (after (first sets) "name=" " ") (rest sets))
    :else nil))
    
(defn retObjs [filename triggers]
  "Returns the list of objects associated with the filename."
  (loop [objs (file-split filename "EOL"), robjs '()]
    (loop [inners (lines-between (first objs) "{" "}"), r1 '()]
      (choose (
               (make-impl-obj (after (first (first objs)) "name=" " ") (gen-obj-list inners ":" false))
               (make-expl-obj (after (first (first objs)) "name=" " ") (gen-obj-list inners ":" true))
               (make-instantiation (after (first (first objs)) "name=" " ") (after (first (first objs)) "parent=" " "))
      (cond
        (.contains (first (first objs)) "implicit")
                    (make-impl-obj
      
