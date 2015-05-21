(defvar *f* nil) ;foil
(defvar *t* nil) ;target
(defvar *c-f* nil) ;correct foil
(defvar *c-t* nil) ;correct target
(defvar *e-f* nil) ;error foil
(defvar *e-t* nil) ;error target
(defvar *forget* 0)
(defvar *skip* 0) 
(defvar *react* 0)

(defun give-feedback-correct-target()
  (cond (*f* 
       (setf *e-f* t)
       (setf *f* nil)
       (clear-exp-window)
       (add-text-to-exp-window :text "error" :x 25 :y 50)
       (proc-display))
      (*t* 
       (setf *c-t* t)
       (setf *t* nil)
       (clear-exp-window)
       (add-text-to-exp-window :text "correct" :x 25 :y 50)
       (proc-display))))

(defun give-feedback-correct-foil()
  (cond (*f* 
         (setf *c-f* t) 
         (setf *f* nil)
         (clear-exp-window)
         (add-text-to-exp-window :text "correct" :x 25 :y 50)
         (proc-display))
        (*t* 
         (setf *e-t* t)
         (setf *t* nil)
         (clear-exp-window)
         (add-text-to-exp-window :text "error" :x 25 :y 50)
         (proc-display))))

(defun incr-forget()
  (cond (*f* 
         (setf *e-f* t) 
         (setf *f* nil))
        (*t* 
         (setf *e-t* t)
         (setf *t* nil)))
  (incf *forget*))

(defun incf-skip ()
  (cond (*f* 
         (setf *c-f* t)
         (setf *f* nil))
        (*t* 
         (setf *e-t* t)
         (setf *t* nil)))
  (incf *skip*))

(clear-all)

(define-model N-Back-LC25

(sgp :esc t :trace-detail low :v nil	 
:ACT nil
:lf .1
:bll .5 
:egs 1.5
:ans .05
:rt                      -0.35 ;   was -0.35
:TIME-NOISE              0.015 ;   default: 0.015      : Temporal noise
:MAS                         3 ;   default: NIL        : Maximum Associative Strength
:DECLARATIVE-FINST-SPAN   10.0 ;   default: 3.0        : Duration of declarative finst markers in seconds
:GA                        1.5 ;   default: 1          : source spread for the GOAL buffer
:UL                          t ;   default: NIL
:ALPHA                     0.2 ;   default: 0.2        : Production learning rate
)

(chunk-type spot-targets time lag state)
(chunk-type estimation time value judgment)
(chunk-type number string value)

(add-dm (detect isa chunk))
(add-dm (attend isa chunk))
(add-dm (estimate isa chunk))
(add-dm (feedback isa chunk))
(add-dm (read isa chunk))
(add-dm (update isa chunk))
(add-dm (decide isa chunk))
(add-dm (react isa chunk))
(add-dm (compute-time isa chunk))
(add-dm (one isa chunk))
(add-dm (two isa chunk))
(add-dm (three isa chunk))
(add-dm (four isa chunk))
(add-dm (five isa chunk))

(add-dm (n1 isa number string n1 value 1))
(add-dm (n2 isa number string n2 value 2))
(add-dm (n3 isa number string n3 value 3))
(add-dm (n4 isa number string n4 value 4))
(add-dm (n5 isa number string n5 value 5))
(add-dm (n6 isa number string n6 value 6))
(add-dm (n7 isa number string n7 value 7))
(add-dm (n8 isa number string n8 value 8))
(add-dm (n9 isa number string n9 value 9))
(add-dm (n10 isa number string n10 value 10))
(add-dm (n11 isa number string n11 value 11))
(add-dm (n12 isa number string n12 value 12))
(add-dm (n13 isa number string n13 value 13))
(add-dm (n14 isa number string n14 value 14))

;;; time estimations 

(add-dm (e21 isa estimation time n4 value two judgment "correct"))

(set-all-base-levels 10000000 -1000)

(P attend-letter
  =goal>
     ISA         spot-targets
     state       attend
  =visual-location>
     ISA         visual-location
  ?visual>
     state       free
  ?temporal>
     state       free
==>
  +visual>
     ISA         move-attention
     screen-pos  =visual-location
  +temporal>
     isa         time
  =goal>
     state       detect
)

(P retrieve-repeated-letter
  =goal>
     ISA         spot-targets
     state       detect
  =visual>
     isa         text
     value       =letter
     status      nil
  =temporal>
     isa         time
     ticks       =ticks
  ?retrieval>
     state       free
  ?vocal>
     state       free
==>
  =temporal>
  =visual>
     status      =ticks
  +retrieval>
     isa         text  
     value       =letter
     < status    =ticks
     :recently-retrieved nil
  +vocal>
     isa         subvocalize 
     string      =letter
  =goal>
     state       compute-time
;   !eval!         (if *t* (format t "target")(if *f* (format t "foil")))
)

(P listen-letter
  =goal>
     ISA         spot-targets
  =aural-location>
     ISA         audio-event
  ?aural>
     state       free
==>
  +aural>
     ISA         sound
     event       =aural-location
)

(P score-failure-to-detect-repetition
  =goal>       
     isa         spot-targets
     state       compute-time
  ?retrieval>
     state       error
;   !eval! (or *f* *t*)
==>
  =goal>       
     state       detect
  !eval! (incf-forget) ;;(if *f* (progn (setf *e-f* t)(setf *f* nil)(incf *forget*))(if *t* (progn (setf *e-t* t)(setf *t* nil)(incf *forget*))))
)

(P compute-interval
  =goal>       
     isa         spot-targets
     state       compute-time
  =retrieval>
     isa         text
     status      =old
  =visual>
     isa         text
     status      =new
  ?retrieval>
     state       free
==>
  !bind! =time (- =new =old) 
  +retrieval>
     isa         number  
     value       =time
  =goal>       
     state       estimate
  !output!       =old
  !output!       =new
)

(P fail-to-compute-interval
  =goal>       
     isa         spot-targets
     state       estimate
  ?retrieval>
     state       error
==>
  =goal>       
     time        nil
     state       react
)

(P retrieve-estimation
  =goal>       
     isa         spot-targets
     state       estimate
  =retrieval>
     isa         number
     string      =time
  ?retrieval>
     state       free
==>
  +retrieval>
     isa         estimation  
     time        =retrieval
     judgment    "correct"
  =goal>       
     time        =time 
     state       decide
)

(P retrieve-new-estimation
  =goal>       
     isa         spot-targets
     time        =time 
     state       decide
  ?retrieval>
     state       error
==>
  +retrieval>
     isa         estimation  
     time        =time
     judgment    "correct"
  =goal>       
     state       react
)

(P react-to-repetition-as-it-were-target
  =goal>       
     isa         spot-targets
     time        =time
     lag         =lag
     state       react
  ?retrieval>
     state       error
  ?manual>
     state       free
  ?imaginal>
     state       free
==>
  +imaginal>
     isa         estimation
     time        =time 
     value       =lag
     judgment    "correct"
  +manual>
     isa         press-key
     key         "M"
  !eval! (geive-feedback-correct-target) ;;(if *f* (progn (setf *e-f* t)(setf *f* nil)(clear-exp-window)(add-text-to-exp-window :text "error" :x 25 :y 50)(proc-display))(if *t* (progn (setf *c-t* t)(setf *t* nil)(clear-exp-window)(add-text-to-exp-window :text "correct" :x 25 :y 50)(proc-display))))
  =goal>       
    state        feedback
    !eval! (incf *react*)
)

(P react-to-repetition-as-it-were-foil
  =goal>       
     isa         spot-targets
     time        =time
     lag         =lag
     state       react
  ?retrieval>
     state       error
  ?manual>
     state       free
  ?imaginal>
     state       free
==>
  +imaginal>
     isa         estimation
     time        =time 
     value       =lag
     judgment    "incorrect"
  +manual>
     isa         press-key
     key         "Z"
  =goal>       
    state        feedback
  !eval! (incf-skip) ;(if *f* (progn (setf *c-f* t)(setf *f* nil))(if *t* (progn (setf *e-t* t)(setf *t* nil))))
  ;!eval! (incf *skip*)
)

(P find-target
  =goal>       
    isa          spot-targets
    lag          =lag
    state        decide
  =retrieval>
    isa          estimation
    value        =lag
    judgment     "correct"
  ?manual>
    state        free
==>
  =retrieval>
  +manual>
    isa          press-key
    key          "M"
  !eval! (give-feedback-correct-target) ;(if *f* (progn (setf *e-f* t)(setf *f* nil)(clear-exp-window)(add-text-to-exp-window :text "error" :x 25 :y 50)(proc-display))(if *t* (progn (setf *c-t* t)(setf *t* nil)(clear-exp-window)(add-text-to-exp-window :text "correct" :x 25 :y 50)(proc-display))))
  =goal>       
    state        feedback
)

(P find-foil
  =goal>       
    isa          spot-targets
    lag          =lag
    state        decide
  =retrieval>
    isa          estimation
    - value      =lag
    judgment     "correct"
  ?manual>
    state        free
==>
  =retrieval>
  +manual>
    isa          press-key
    key          "Z"
  !eval! (give-feedback-correct-foil) ;; (if *f* (progn (setf *c-f* t)(setf *f* nil)(clear-exp-window)(add-text-to-exp-window :text "correct" :x 25 :y 50)(proc-display))(if *t* (progn (setf *e-t* t)(setf *t* nil)(clear-exp-window)(add-text-to-exp-window :text "error" :x 25 :y 50)(proc-display))))
  =goal>       
    state        feedback
)

(P find-foil-in-aural 
  =goal>
     ISA         spot-targets
     lag         two
     state       detect
  =visual>
     isa         text
     value       =letter
     status      nil
  =aural>
     isa         sound
     content     =letter
  ?manual>
    state        free
  !eval! *f*
==>
  +manual>
    isa          press-key
    key          "Z"
  !eval! (progn (setf *c-f* t)(setf *f* nil))
;   !eval!        (format t "foil")
)

(P attend-feedback
  =goal>
     isa         spot-targets
     state       feedback
  ?visual-location>
     buffer      unrequested 
==>
  +visual-location>
     ISA         visual-location
  =goal>
     state       read
)

(P read-feedback
  =goal>
    isa          spot-targets
    state        read
  =visual-location>
     ISA         visual-location
  ?visual>
     state        free
==>
  +visual>
     ISA         move-attention
     screen-pos  =visual-location
  =goal>
     state       update
)

(P update-estimation-in-retrieval
  =goal>       
    isa          spot-targets
    state        update
  =visual>
    isa          text
    value        =feedback
  =retrieval>
    isa          estimation
==>
  =retrieval>
    judgment     =feedback
  =goal>       
    state        attend
)

(P update-estimation-in-imaginal
  =goal>       
    isa          spot-targets
    state        update
  =visual>
    isa          text
    value        =feedback
  =imaginal>
    isa          estimation
==>
  =imaginal>
    judgment     =feedback
  =goal>       
    state        attend
)

(spp :u 100)
(spp (find-foil-in-aural :u 100))

(spp (react-to-repetition-as-it-were-foil :u 80))

(spp (update-estimation-in-retrieval :reward 75))
(spp (update-estimation-in-imaginal :reward 75))

(setf *actr-enabled-p* t)
)