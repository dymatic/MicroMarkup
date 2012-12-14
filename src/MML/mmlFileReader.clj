(ns mml.mmlReader
  (use 'lib.libFiles)
  (use 'lib.libLists)
  (use 'lib.libStrings)
  (use 'mml.mmlParse))
;Reads microMarkup files, returning objects.


(defn ref-obj [master-list name]
  "Emerge and object from the master list."
  (cond
    (= master-list ()) nil
    (= (first (first master-list)) name) (first master-list)
    :else
    (recur (rest master-list) name)))

(defn choose [fl bl]
"Choose given a function list and a boolean list"
  (cond
    (or (= bl ()) (= fl ())) nil
    (first bl) (first fl)
    :else (recur (rest fl) (rest bl))))

;LIST FORMAT: '((x(a b) (y z)))
(defn read-file [filename];Fix this function later
  "Returns a list of objects found within the file."
  (loop [col '(), ll (lines filename)]
    (if (= ll ()) (remove-from col nil)
      (recur (conj col (let [n (if (.contains (first ll) "named ") (.replace (after (first ll) "named ") "{" "") (first ll)),
                             s (between ll 0 "{" "}")]
                      (cond
                        (.contains (first ll) "explicit") 
                          (make-expl-list n
                                          s)

                         (found-from? '("implicit" "template") (first ll))
                           (do (println "N: " n) (make-impl-list n
                                           s))

                        (.contains (first ll) "instance")
                         (make-struct-list 
                           (.replace (after (first ll) "named " " ") "{" "")
                           s
                           (ref-obj col (.replace (after (first ll) "from ") "{" "")))
                         :else nil)))
             (rest ll)))))
                                 
                                                    
               
               