(ns mml.mmlReader)
;Reads microMarkup files, returning objects.


(defn ref-obj [master-list name]
  "Emerge and object from the master list."
  (cond
    (bad? master-list) nil
    (= (first (first master-list)) name) (first master-list)
    :else
    (recur (rest master-list) name)))
    

;LIST FORMAT: '((x(a b) (y z)))
(defn read-file [filename];Fix this function later
  "Returns a list of objects found within the file."
  (loop [col '(), ll (lines filename)]
    (if (bad? ll) (remove-from col nil)
      (recur (conj col (cond
                         (.contains 
                           (first ll) 
                           "explicit") (make-expl-list
                                         (.replace (after (first ll)
                                                "named ") "{" "")
                                         (between ll
                                                  0
                                                  "{"
                                                  "}"))
                         (found-from? '("implicit" "template")
                                      (first ll))
                         (make-impl-list (.replace (after (first ll)
                                                "named ") "{" "")
                                         (between ll
                                                  0
                                                  "{"
                                                  "}"))
                         (.contains (first ll) "instance")
                         (make-struct-list
                           (.replace (after (first ll)
                                  "named " " ") "{" "")
                           (between ll
                                    0
                                    "{"
                                    "}")
                           (ref-obj col (.replace (after (first ll) "from ") "{" "")))
                         :else nil))
             (rest ll)))))
                                 
                                                    
               
               