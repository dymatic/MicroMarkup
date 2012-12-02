;This library is for parsing the microMarkup language.
;microMarkup is designed to represent Clojure and its
;data types very closely, so Clojure is a good choice in parsing it.

;This library, just as with the entire project, is governed by the GNU LGpl V2
;Creator: Norton "Dymatic" Jenkins
;Conception: 10-17-12

(ns mml.mmlParse)

(import lib);Mainly used for string manipulation

(defn apply-type [x & y]
  "Apply the correct type of x as specified."
  (if (string? x)
  (cond
    (.contains x " As double") (Double/parseDouble (.replace x " As double" ""))
    (.contains x " As String") (str (.replace x " As String" ""))
    (.contains x " As float")  (Float/parseFloat (.replace x " As float" ""))
    (.contains x " As int")    (Integer/parseInt (.replace x " As int" ""))
    (.contains x " As ") (.replace x " As " "")
    :else x) x))

;Make an explicit mapping of an object when given the name and a list of values.
;LIST FORMAT: ((:x y) (:a b))
(defn make-expl-obj [name lis]
"Makes an explicit object from the name and the list of items"
	(loop [ll  lis, col '{}]
   (if (bad? ll) 
     (list name col)
     (recur 
       (rest ll)
       (assoc col (keyword (first (first ll))) (apply-type (second (first ll))))))))


;Makes an implicit named list from the file set.
;LIST FORMAT: ()
(defn make-impl-obj [name lists]
"Makes an implicit object from the name and the list of items"
	(list name (map apply-type lists)))

;Make a structure based on an implicit object and a list of values.
(defn make-struct [name parent vallist]
"Makes an explicit object where the names of a template are the lookup keys."
(loop [pl (second parent), vl vallist, col '()]
  (if (empty? pl) (make-expl-obj name col);((x y) (x y))
    (recur (rest pl) 
           (rest vl)
           (conj col (list (first pl) (apply-type (first vl))))))))

(defn make-expl-list [name set]
  "Parses the strings of a set that would be found in a file to make an explicit object."
  (make-expl-obj name (loop [ll set, col '()]
                        (if (bad? ll) col
                          (recur (rest ll)
                                 (conj col (list (after (first ll) ":" " ")  (after (first ll) "-"))))))))

(defn make-impl-list [name set]
  "Makes an implicit object or a structure based on input"
  (make-impl-obj name (loop [ll (rlist set), col '()]
                        (if (bad? ll) col 
                          (recur (rest ll)
                                 (conj col (apply-type (after (first ll) "-"))))))))

(defn make-struct-list [name set from]
  "Make a structure with the name, a set, and the object which it comes from"
  (make-struct name from (loop [ll set, col '()]
                      (if (bad? ll) (rlist col)
                        (recur (rest ll)
                               (conj col (after (first ll) "-")))))))