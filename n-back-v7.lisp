;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author:	Mike Schoelles
;;; Email: 	schoem@rpi.edu
;;; 
#|
Copyright (c) 2010 CogWorks Laboratory

All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(N.B. this is the MIT license)

|#
;;; Project: N-BACK
;;;
;;; version 1.0
;;;


(setf editor:*maximum-ordinary-windows* 3)


(defun permute-list (lst)
  (let* ((res nil) (accum nil)
         (index-list (dotimes (i (length lst) res) (push i res))))
    (dotimes (i (length lst) accum)
      (let ((idx (nth (random (length index-list)) index-list)))
        (push (nth idx lst) accum)
        (setf index-list (remove idx index-list))))))

(defconstant +passcode+ "cogworks")
(defconstant +window-size+ 800)
(defconstant +letter-size+ 34)
(defparameter +screen-width+ (capi:screen-width (capi:convert-to-screen)))
(defparameter +screen-height+ (capi:screen-height (capi:convert-to-screen)))
(defconstant +cross-size+ 34)
(defconstant +ISI-MIN+ 1)

(defconstant +Letter-Set+ '(d f h l  n r v x )) ;;'(b d g j  q s v x ))
(defconstant +Number-Set+ '(1 2 3 4 5 6 7 8 ))

(defparameter +random-stimulus+ t) ;;mixed up L and N else even L odd N or vice versa
(defparameter +ISI+ 3)
(defparameter +ISI-adjustment+ nil)
(defparameter +Number-Back+ 2)
;(defparameter +%-Match-Trials-Letter+ 30)  
;(defparameter +%-Match-Trials-Number+ 30) 
(defparameter +number-match+ 20)
(defparameter +num-blocks+ 12)
(defparameter +num-trials+ 60)
(defparameter +match-letter+ #\z)
(defparameter +no-match-letter+ #\x)
(defparameter +match-number+ #\z)
(defparameter +no-match-number+ #\x)
(defparameter +ran-constraint+ 10)

(defparameter +num-lure-1+ 5)
(defparameter +num-lure-3+ 5)

(defparameter +num-practice-trials+ 21) ;;per mini block
(defparameter +num-practice-blocks+ 4)
(defparameter +num-practice-match+ 7)
(defparameter +num-practice-lure-1+ 2)
(defparameter +num-practice-lure-3+ 2)

(defparameter +block-order+ '(psl psn pdbL pdbN sl sn dbL dbN sn dbN sl dbL sl dbN sn dbL))

(defclassic block-type ()
 txt
 stimulus
 offset
 num-trials
 num-match
 num-lure-1
 num-lure-3
)

(defclassic real-block (block-type))
(defclassic practice-block (block-type))
(defclassic single-stim () (offset 2))
(defclassic double-stim () (offset 4))
(defclassic sl (real-block single-stim) (stimulus 'L) (offset 2) (txt "Single Letter"))
(defclassic sn (real-block single-stim) (stimulus 'N)  (offset 2) (txt "Single Number"))
(defclassic dbL (real-block double-stim) (stimulus '(L N)) (offset 4) (txt "Double"))
(defclassic dbN (real-block double-stim) (stimulus '( N L)) (offset 4) (txt "Double"))
(defclassic psl (practice-block single-stim) (stimulus 'L) (offset 2) (txt "Practice Single Letter"))
(defclassic psn (practice-block single-stim ) (stimulus 'N) (offset 2) (txt "Practice Single Number"))
(defclassic pdbL (practice-block double-stim) (stimulus '(L N)) (offset 4) (txt "Practice Double"))
(defclassic pdbN (practice-block double-stim) (stimulus '( N L)) (offset 4) (txt "Practice Double"))




(defclassic n-back-trial ()
 blk
 blk-num 
 trial-num
 modality
 stimulus
 value
 (distractor? t)
 (lure nil)
 lure-target
 expected-response
 response-time 
 actual-response
 practice-cnd)

(defclassic n-back-condition () 
  cnd-message
  end-of-blk-msg
  %-correct)

(defclassic pure-nback-condition (n-back-condition))
(defclassic pure-nback-visual (pure-nback-condition))
(defclassic dual-nback-condition (n-back-condition))
(defclassic pure-cnd (dual-nback-condition))
(defclassic mixed-cnd (dual-nback-condition))
(defclassic pure-dual-visual (pure-cnd)
  (cnd-message "Visual letters and Visual Numbers "))
(defclassic pure-dual-audio (pure-cnd)
  (cnd-message "Audio letters and Audio Numbers "))
(defclassic mixed-visual-letter (mixed-cnd)
  (cnd-message "Visual letters and Audio Numbers "))
(defclassic mixed-audio-letter (mixed-cnd)
  (cnd-message "Audio letters and Visual Numbers "))
;(defclassic practice (n-back-condition) exp-cnd typ)

(defmethod set-eeg-event-label (eid (tr n-back-trial))
  (with-slots (blk modality stimulus lure expected-response) tr
    (format nil "~S~S~S~S~S" (get-eeg-code eid ) (get-eeg-code modality) (get-eeg-code stimulus) (get-eeg-code (read-from-string (txt blk)))
            (aif (get-eeg-code expected-response) it (get-eeg-code lure)))))

(let ((symbol->eeg-code '((visual 1) (audio 2) (L 1) (N 2) (Single 1) (Double 2) (Practice 3) (Match 1) (1 2) (3 3) (nil 4)
                          (stimulus  1) (response 2) (error 3))))
  (defun get-eeg-code (sym) (second (find sym symbol->eeg-code :key 'first)))
  (defun eeg-event-notify (eid tr-obj)
#+:eeg (when (eeg-p) (eeg:event-notify (get-eeg-code eid) :label (set-eeg-event-label eid tr-obj))
         (log-info `(eeg-event ,(get-eeg-code eid) ,(set-eeg-event-label eid tr-obj))))
 )
)
    
(defclassic n-back ()
  (num-blocks +num-blocks+)
  (num-trials +num-trials+)
  blocks
  trials
  exp-win
  (exp-cnd (make-instance 'pure-dual-visual))
  (exp-name 'Nback)
  (load-path (current-pathname))
  task
  current-trials
  in-progress? 
  btn-box
  )

(let ((n-back (make-instance 'n-back )))
  (defun n-back () n-back))

(defmethod make-blocks ((p n-back))
  (dolist (blk +block-order+)
    (push (make-instance blk) (blocks p)))
  (setf (blocks p) (reverse (blocks p))))

(defmethod initialize-instance :after ((blk practice-block) &rest args)
  (with-slots (num-trials num-match num-lure-1 num-lure-3) blk
  (setf num-trials +num-practice-trials+ num-match +num-practice-match+
        num-lure-1 +num-practice-lure-1+ num-lure-3 +num-practice-lure-3+ )))

(defmethod initialize-instance :after ((blk real-block) &rest args)
  (with-slots (num-trials num-match num-lure-1 num-lure-3) blk
  (setf num-trials +num-trials+ num-match +number-match+
        num-lure-1 +num-lure-1+ num-lure-3 +num-lure-3+ )))
   
(defun make-random-stims (&key (num +ran-constraint+) (groups 2))
  (let ((l-list (make-list (floor num 2) :initial-element 'L))
        (n-list (make-list (floor num 2) :initial-element 'N))
        (res nil))
    (dotimes (i groups res)
      (setf res (append res (permute-list (append l-list n-list)))))))

(defmethod make-trial ((cnd pure-dual-visual) i j)
  (make-instance 'n-back-trial :blk-num i :trial-num j :modality 'visual :stimulus (if (evenp j) 'L 'N)  ))

(defmethod make-trial ((cnd pure-dual-audio) i j)
  (make-instance 'n-back-trial :blk-num i :trial-num j :modality 'audio :stimulus (if (evenp j) 'L 'N)  ))

(defmethod make-trial ((cnd mixed-visual-letter) i j)
  (make-instance 'n-back-trial :blk-num i :trial-num j :modality   'visual  :stimulus (if (evenp j) 'L 'N) ))

(defmethod make-trial ((cnd mixed-audio-letter) i j )
  (make-instance 'n-back-trial :blk-num i :trial-num j :modality  'audio  :stimulus (if (evenp j) 'N 'L)  ))

(defmethod make-trial ((cnd pure-nback-visual) i j )
  (make-instance 'n-back-trial :blk-num i  :trial-num j :modality 'visual))

(defun calc-lure-level (tr trs)
  (aif (position (value tr)  (mapcar 'value (reverse (subseq trs 0 (position tr trs))))) (1+ it)))

(defmethod set-trial-stimulus ((blk single-stim)  trs)
  (let ((typ (type-of (exp-cnd (n-back)))))
    (dolist (tr trs) 
      (setf (stimulus tr) (stimulus blk))
      (cond ((and (eql (stimulus tr) 'N) (eql typ 'mixed-visual-letter) )
             (setf (modality tr) 'audio ))
            ((and (eql (stimulus tr) 'N) (eql typ 'mixed-audio-letter) ) 
             (setf (modality tr) 'visual ))))))

(defmethod set-trial-stimulus ((blk double-stim) trs)
  (let ((stim-list (make-random-stims :groups (ceiling (num-trials blk) +ran-constraint+)))
        (trss (subseq trs (offset blk)))
        (typ (type-of (exp-cnd (n-back)))))
    (dotimes (i (length trss))
      (setf (stimulus (nth i trss)) (nth i stim-list)))
    (dolist (tr trs)
      (cond ((and (eql (stimulus tr) 'N) (eql typ 'mixed-visual-letter) )
             (setf (modality tr) 'audio ))
            ((and (eql (stimulus tr) 'N) (eql typ 'mixed-audio-letter) )
             (setf (modality tr) 'visual ))))))

(defmethod make-trials ((p n-back))
  (with-slots (blocks exp-cnd trials) p
    (let ((trial-accum nil))
      (dolist (blk blocks)
        (with-slots (num-trials num-match num-lure-1 num-lure-3 stimulus offset) blk
          (setq trial-accum nil)
          (dotimes (j (+ num-trials offset))
            (push (make-trial exp-cnd (position blk blocks) j) trial-accum)
            (setf (blk (first trial-accum)) blk))
          (setq trial-accum (reverse trial-accum))
          (set-trial-stimulus blk trial-accum)
          (set-trial-values trial-accum num-match offset :lures1 num-lure-1 :lures3 num-lure-3)
          ;(dotimes (i (+ num-trials offset) (setf (trial-num (nth i trial-accum)) i))
          (dolist (tr trial-accum)
            (with-slots (value expected-response stimulus blk-num modality trial-num lure distractor?) tr
              (log-info `(trial-init :blk-num ,blk-num :trial-num ,trial-num  :modality ,modality :stimulus ,stimulus 
                              :value ,value :expected-response ,expected-response :distractor ,distractor? :lure ,lure
                              :lure-level ,(calc-lure-level tr trial-accum) :blk-type ,(type-of blk)))))
          (setf trials (append trials trial-accum)))))))

(defun get-a-value-ne (tr)
  (case (stimulus tr)
    (L
     (let ((s (remove (value tr) +letter-set+)))
       (nth (random (length s)) s)))
    (N
     (let ((s (remove (value tr) +number-set+)))
       (nth (random (length s)) s)))))

(defun get-a-value ( tr)
  (case (stimulus tr)
    (L (nth (random (length +letter-set+)) +letter-set+))
    (N (nth (random (length +number-set+)) +number-set+))))

(defun find-tr-nback (i trs stim num-back) 
  (let ((temp (reverse (subseq trs 0 i)))  
         (cnt 0))
    (dotimes (j (length temp))
      (if (eql stim (stimulus (nth j temp))) (incf cnt))
      (when (eql cnt num-back) 
        (return (nth j temp))))))

(defun test-ne-p (trs* id* val)  
  (let ((tr1* (nth (+ id* 1) trs*))
        (tr2* (nth (+ id* 2) trs*))
        (tr3* (nth (+ id* 3) trs*)))
    (and tr1* (not (eql val (value tr1*)))
         tr2* (not (eql val (value tr2*)))
         tr3* (not (eql val (value tr3*))))))

(defun get-forward-vals (trs* id*)
  (let ((trs (list (nth (+ id* 1) trs*) (nth (+ id* 2) trs*) (nth (+ id* 3) trs*))))
    (remove-duplicates (mapcar 'value (remove nil trs)))))

(defun get-possible-values (trs* id* val)
  (let ((item (nth (+ id* 1) trs*))
        (vals (cons val (get-forward-vals trs* id*))))
  (when item
    (if (numberp val)  (set-difference +number-set+ vals) (set-difference +letter-set+ vals)))))
        
        

(defun set-lures (trs match-ids num1 num3 ) ;;n 1 or 3, num = number of lures to generate
  (let ((lure-ids nil) )
     (dotimes (i (length trs))
      (if (eql (expected-response (nth i trs)) 'no-match)  (push i lure-ids)))
     (setq lure-ids (permute-list lure-ids))
     (log-info `(trial-init :lure-ids ,lure-ids))
     (dolist (id  (copy-list lure-ids))
       (let* ((n nil)
              (tr (nth id trs))
              (trs* (remove-if-not (lambda(x) (eql (stimulus x) (stimulus tr))) trs))
              (id* (position tr trs*)))
         (cond ((and (plusp id*) (null (lure tr)) 
                     (eql (value tr) (value (nth (- id* 1) trs*))))
                (cond ((plusp num1)
                       (decf num1)
                       (setf n 1))
                      ((and (null (lure-target tr)) (null (lure tr))
                            (not (member id match-ids)))
                       (let ((vals (get-possible-values trs* id* (value tr))))
                         (if vals (setf (value tr) (nth (random (length vals)) vals)))))))
               ((and (null (minusp (- id* 3)))  (null (lure tr))  
                     (eql (value tr) (value (nth (- id* 3) trs*))))
                (cond ((plusp num3)
                       (decf num3)
                       (setf n 3))
                      ((and (null (lure-target tr)) (null (lure tr))
                            (not (member id match-ids)))
                       (let ((vals (get-possible-values trs* id* (value tr))))
                         (if vals (setf (value tr) (nth (random (length vals)) vals))))))))
         (when n
           (log-info `(trial-init :lure ,id :val ,(value tr) :typ ,n))
           (setf (lure tr) n)
           (setf lure-ids (remove id lure-ids))
           (setf (lure-target (nth (- id* n) trs*)) t))))
     (log-info `(trial-init :lure-ids ,lure-ids))
     (dolist (lure `((1 ,num1) (3 ,num3)))
       (let ((n (first lure)) (num (second lure)) (cnt 0))
         (when (plusp num)
           (while (and lure-ids (< cnt num))
                (let* ((id (pop lure-ids))
                       (tr (nth id trs))
                       (trs* (remove-if-not (lambda(x) (eql (stimulus x) (stimulus tr))) trs))
                       (id* (position tr trs*))
                       (lure-targ-id  (if (null (minusp (- id* n))) (- id* n )))
                       (lure-targ (if lure-targ-id (nth lure-targ-id trs*)))
                       (val (if lure-targ (value lure-targ ))))
                  (when (and lure-targ (null (lure-target tr)) (null (lure tr))
                             (not (member id match-ids)) 
                             (or (null (nth (- id* 2) trs*)) (not (eql val (value (nth (- id* 2) trs*)))))     
                             (test-ne-p trs* id* val))     
                        (log-info `(trial-init :lure ,id :val ,val :typ ,n))
                        (incf cnt)
                        (setf (value tr) val)
                        (setf (lure tr) n)
                        (setf (lure-target  lure-targ) t)))))))))
         

             
               
        
(defun get-non-lure-value (trs tr &key (nval nil))
  (let* ((trs* (remove-if-not (lambda(x) (eql (stimulus x) (stimulus tr))) trs))
         (id* (position tr trs*))
         (fvals (get-forward-vals trs* id*))
         (vals  (remove nval (set-difference (if (eql (stimulus tr) 'L) +letter-set+ +number-set+) fvals)))
         (val (nth (random (length vals)) vals)))
      (if (null val) (break))
      val))
                           
(defun set-trial-values (trs num-match offset &key (lures1 nil) (lures3 nil)) ;;typ =1 single nback, =2 double nback
  (let* ((tr nil)
        (match-ids nil)
        (match-ids2 nil)
        (num-ids (- (length trs)  offset)))
    (while (< (length match-ids) num-match)
           (push (+ offset (random num-ids)) match-ids)
           (setq match-ids (remove-duplicates match-ids)))
    (log-info `(trial-init :match-ids ,match-ids))
    (dotimes (i (length trs))
      (setf tr (nth i trs))
      (with-slots (value expected-response stimulus) tr
        (let ((prev-tr (if (>= i offset) (find-tr-nback i trs stimulus +number-back+))))
          (cond ((member i match-ids)
                 (cond ((and prev-tr (value prev-tr))
                        (setf value (value prev-tr))
                        (setf (distractor? prev-tr) nil))
                       (prev-tr
                        (setf value (get-non-lure-value trs tr)) ;;;(get-a-value tr))
                        (setf (value prev-tr ) value) 
                        (setf (distractor? prev-tr) nil)))
                 (push (position prev-tr trs) match-ids2)
                 (setf expected-response 'match))
                ((>= i offset)
                 (cond ((and prev-tr (value prev-tr))
                        (setf value (get-non-lure-value trs tr :nval (value prev-tr) ))) ;;;(get-a-value-ne prev-tr)))
                       (prev-tr 
                        (setf value (get-non-lure-value trs tr)) ;(get-a-value tr))
                        (setf (value prev-tr) (get-non-lure-value trs tr :nval value )) ;;;;(get-a-value-ne tr))
                        (setf (expected-response prev-tr) 'no-match)))
                 (setf expected-response 'no-match))))))

    (dotimes (i offset)
      (if (null (value (nth i trs))) (setf (value (nth i trs)) (get-a-value-ne (nth (+ i offset) trs))))
      (setf (expected-response (nth i trs)) nil)
      )
    (setf match-ids (append match-ids match-ids2))
    (set-lures trs match-ids lures1 lures3 )
   
    (log-info `(trial-init :targets ,(count 'match trs :key 'expected-response) :distractors ,(count t trs :key 'distractor?) 
                           :lure1 ,(count 1 trs :key 'lure ) :lure3 ,(count 3 trs :key 'lure )))
    ))

    
(defclass cross (capi:drawn-pinboard-object)
 ((color :initform :black :accessor color))
  (:default-initargs 
   :x (- (floor +window-size+ 2) (floor +cross-size+ 2))
   :y (- (floor +window-size+ 2) (floor +cross-size+ 2))
   :internal-min-width +cross-size+
   :internal-min-height +cross-size+
   :internal-max-width  +cross-size+
   :internal-max-height +cross-size+  
   :display-callback 'draw-cross
   
   ))

(capi:define-interface n-back-screen () 
  ()
  (:panes
   (cross-display cross :accessor cross)
   (disp capi:item-pinboard-object
         :x (- (floor +window-size+ 2) (floor +letter-size+ 2))
         :y (- (floor +window-size+ 2) (floor +letter-size+ 2))
         :visible-min-width +letter-size+
         :visible-min-height +letter-size+
         :text ""
         :accessor disp))
  (:layouts
   (screen capi:pinboard-layout '(disp) 
           :visible-min-width +window-size+ 
           :visible-min-height +window-size+ 
           :input-model `((:character char-input-callback ))
           :font (gp:make-font-description
                  :family "times" 
                  :size +letter-size+ 
                  :weight :medium
                  :slant :roman)
           :accessor screen))
  (:default-initargs
   :title "N-Back"
   :window-styles '(:borderless :always-on-top)
   
   :layout 'screen))

;;----------------------------------------------------------------------------
;; Define a square pinboard-object
;;----------------------------------------------------------------------------

(defclass square (capi:pinboard-object)
  ((foreground :accessor foreground :initform nil :initarg :foreground)
   (filled :accessor filled :initform nil :initarg :filled))
  (:default-initargs
   :x (- (floor +window-size+ 2) 17)
   :y (- (floor +window-size+ 2) 17)
   :visible-min-width 34
   :visible-min-height 34))

(defmethod capi:draw-pinboard-object (pinboard (square square) &key)
  (capi:with-geometry square
    (let ((filled? (filled square)))
      (log-info `(display-feedback :feedback ,(foreground square) :x ,capi:%x% :y ,capi:%y% :size ,capi:%width%))
      (gp:draw-rectangle pinboard
                         capi:%x% capi:%y%
                         (if filled? capi:%width% (1- capi:%width%))
                         (if filled? capi:%height% (1- capi:%height%))
                         :foreground (or (foreground square)
				         (capi:simple-pane-foreground pinboard))
                         :filled filled?))))

(defmethod display-feedback ((interface n-back-screen) tr)
  (with-slots(expected-response actual-response) tr
    (let ((sq (make-instance 'square :filled t 
                             :foreground (if (null expected-response) 
                                             :yellow 
                                           (if (eql actual-response nil) :black
                                             (if (eql actual-response expected-response) :green :red))))))
      (capi:apply-in-pane-process (screen interface) (lambda(win) (setf (capi:layout-description (screen win)) (list sq))) interface))))

(defmethod calc-actual-response ((tr n-back-trial) key)
  (cond ((and  (eql (stimulus tr) 'L) (eql key +match-letter+) 'match))
        ((and  (eql (stimulus tr) 'N) (eql key +match-number+) 'match))
        ((and  (eql (stimulus tr) 'L) (eql key +no-match-letter+) 'no-match))
        ((and  (eql (stimulus tr) 'N) (eql key +no-match-number+) 'no-match))
        (t  (capi:beep-pane) (sleep .1) (capi:beep-pane)
            'invalid-response)))
     
(defun char-input-callback (self x y key)
  (declare (ignore x y))
  (let ((tr (first (current-trials (n-back)))))
    
       (cond ((null (in-progress? (n-back)))
              (log-info `(subject-input :response ,key :trial-num ,(trial-num tr) :stimulus ,(stimulus tr) :modality ,(modality tr) 
                                        :expected-response ,(expected-response tr) 
                                        :actual-response  ,(if (actual-response tr) 'already-responded (setf (actual-response tr) 'too-late)))))
             ((and (expected-response tr)
                   (or (eql key +match-letter+) (eql key +no-match-letter+) (eql key +match-number+) (eql key +no-match-number+)))
              (log-info `(subject-input :response ,key :trial-num ,(trial-num tr) :stimulus ,(stimulus tr) :modality ,(modality tr) 
                                        :expected-response ,(expected-response tr) 
                                        :actual-response  ,(if (actual-response tr) 'already-responded (setf (actual-response tr)  (calc-actual-response tr key))))))
             (t
              (log-info `(subject-input :response ,key :expected-response ,(expected-response tr) :actual-response invalid-response ))
              (capi:beep-pane self) (sleep .1) (capi:beep-pane self)))
       (if (eql (actual-response tr) 'match) (eeg-event-notify 'response tr) (eeg-event-notify 'error tr))
       ))

;;;
;;;buton box processing
;;;
(defun process-response (btn ts)
  (declare (ignore ts)) 
  (with-slots ( in-progress? current-trials ) (n-back)
    (let ((tr (first current-trials)))
      (when tr
        
        (cond ((and (expected-response tr) (numberp btn) (<= 3 btn 4) (numberp in-progress?) (plusp in-progress?))
               (setf (response-time tr) (- (get-internal-real-time) in-progress?) )
               (log-info `(subj-action :event button-press :response-time ,(response-time tr) :response,btn  ))
               
               (log-info `(subj-input :response ,btn :trial-num ,(trial-num tr) :stimulus ,(stimulus tr)  :modality ,(modality tr) 
                                      :expected-response ,(expected-response tr) 
                                      :actual-response ,(if (actual-response tr) 'already-responded 
                                                          (setf (actual-response tr) (cond ((or (= btn 1) (= btn 3)) 'match)
                                                                                           ((or (= btn 2) (= btn 4)) 'no-match)
                                                                                           (t 'invalid-response)))))))   
              ((null in-progress?)
               (log-info `(subject-input :response ,btn :trial-num ,(trial-num tr) :stimulus ,(stimulus tr) :modality ,(modality tr) 
                                        :expected-response ,(expected-response tr) 
                                        :actual-response  ,(if (actual-response tr) 'already-responded (setf (actual-response tr) 'too-late)))))              
              (t
               (setf (actual-response tr) 'invalid)
               (log-info `(subj-input :response ,btn :trial-num ,(trial-num tr) :stimulus ,(stimulus tr)  :modality ,(modality tr) 
                                      :expected-response ,(expected-response tr) :actual-response invalid-response))))
        (if (eql (actual-response tr) 'match) (eeg-event-notify 'response tr) (eeg-event-notify 'error tr))))))


(defmethod draw-cross (output-pane (self cross) x y w h)
  (let ((f (gp:find-best-font output-pane (gp:augment-font-description 
                   (gp:font-description(gp:graphics-state-font (gp:get-graphics-state output-pane)))
                    :size +cross-size+))))
    (capi:with-geometry self
       (gp:with-graphics-state (output-pane :font f :foreground (color self)) 
         (gp:draw-character output-pane #\+ 
                         (+ capi:%x%  (floor capi:%width% 2)) (+ capi:%y%  (floor capi:%height% 2)))))))

(defmethod display-stimulus ((interface n-back-screen) tr)
  (capi:apply-in-pane-process (screen interface) 
                              (lambda (win) (setf (capi:item-text (disp win)) (write-to-string (value tr))) 
                                (setf (capi:layout-description (screen interface)) '(disp)) 
                                (gp:invalidate-rectangle (screen win))
                                (eeg-event-notify 'stimulus tr)
                                (log-info `(trial display-stimulus :value ,(value tr)))) 
                              interface))

(defmethod clear-stimulus ((interface n-back-screen))
  (capi:apply-in-pane-process (screen interface) (lambda (win) 
                                                   (setf (capi:item-text (disp win)) "") 
                                                   (gp:invalidate-rectangle (screen win))
                                                   (log-info `(trial clear-stimulus)) ) interface))

(defmethod display-cross ((interface n-back-screen))
  (capi:apply-in-pane-process (screen interface) 
                              (lambda () (setf (capi:layout-description (screen interface)) '(cross-display))
                                (log-info `(trial display-cross))
                                (gp:invalidate-rectangle (screen interface)))))

(defmethod clear-screen ((interface n-back-screen))
  (capi:apply-in-pane-process (screen interface) 
                              (lambda () (setf (capi:layout-description (screen interface)) '())
                                (gp:invalidate-rectangle (screen interface))
                                (log-info `(trial clear-screen )))))

(defclassic nback-results ()
 (num-correct-letter-visual 0) 
 (num-correct-number-visual 0)
 (num-correct-letter-audio 0) 
 (num-correct-number-audio 0)
 (total-letter-visual 0)
 (total-number-visual 0)
 (total-letter-audio 0)
 (total-number-audio 0))

(defmethod calc-results ((obj nback-results) trs)
  (with-slots (num-correct-letter-visual num-correct-number-visual num-correct-letter-audio num-correct-number-audio 
               total-letter-visual total-number-visual total-letter-audio total-number-audio) obj
    (let ((visual-trials (remove-if-not (lambda(tr) (and (eql (modality tr) 'visual) (expected-response tr) )) trs))
          (audio-trials (remove-if-not (lambda(tr) (and (eql (modality tr) 'audio) (expected-response tr) ))  trs)))
      (when visual-trials
        (setq num-correct-letter-visual (count-if (lambda (tr) (and (eql (stimulus tr) 'L) (eql (expected-response tr) (actual-response tr)))) visual-trials) )
        (setq total-letter-visual (count-if (lambda(tr) (eql (stimulus tr) 'L)) visual-trials) )
        (setq num-correct-number-visual (count-if (lambda (tr) (and (eql (stimulus tr) 'N) (eql (expected-response tr) (actual-response tr)))) visual-trials) )
        (setq total-number-visual (count-if (lambda(tr) (eql (stimulus tr) 'N)) visual-trials) ))
      (when audio-trials
        (setq num-correct-letter-audio (count-if (lambda (tr) (and (eql (stimulus tr) 'L) (eql (expected-response tr) (actual-response tr)))) audio-trials) )
        (setq total-letter-audio (count-if (lambda(tr) (eql (stimulus tr) 'L)) audio-trials) )
        (setq num-correct-number-audio (count-if (lambda (tr) (and (eql (stimulus tr) 'N) (eql (expected-response tr) (actual-response tr)))) audio-trials) )
        (setq total-number-audio (count-if (lambda(tr) (eql (stimulus tr) 'N)) audio-trials) ))
      (log-info (list 'results num-correct-letter-visual total-letter-visual num-correct-number-visual total-number-visual 
                       num-correct-letter-audio total-letter-audio num-correct-number-audio total-number-audio)) 
                 
      obj)))

(defun calc%correct (nl nn tl tn)
 (if (zerop (+ tl tn)) 0 (floor (* 100.0 (/ (+ nl nn) (+ tl tn)))) ))

(defun l-p (blk)
  (or (eql (stimulus blk) 'L) (and (consp (stimulus blk)) (member 'L (stimulus blk)))))

(defun n-p (blk)
  (or (eql (stimulus blk) 'N) (and (consp (stimulus blk)) (member 'N (stimulus blk)))))

(defmethod end-of-block-msg ((cnd n-back-condition) trs cur-blk next-blk)
  (let ((str1 (format nil "You have completed block(s) ~S of ~S blocks" (1+ (blk-num (first trs))) (+ +num-practice-blocks+ +num-blocks+)))   
        (str2 (format nil "~%~%Your accuracy is ~S~%~%" (%-correct cnd)))
        (str "")
        (str0 (concatenate 'string (cnd-message cnd) (format nil "~%~%~S-back~%~%~%" +number-back+))))
       (setq str (concatenate 'string str1 (end-of-blk-msg cnd) str2  (if (< (%-correct cnd)  90) "~%~%Try to be more accurate"
          (if (>= (%-correct cnd) 95) "~%~%Try to go faster" ""))))
       (setq str (concatenate 'string str0 str (if next-blk (format nil "~%~%Next Block is ~A" (txt next-blk)) (format nil "~%~%No next Block"))))
       (capi:display-message str)
       (%-correct cnd)))

(defmethod end-of-block-msg ((cnd pure-dual-visual) trs cur-blk next-blk)
   (let ((obj (calc-results (make-instance 'nback-results) trs)))
     (with-slots (num-correct-letter-visual total-letter-visual num-correct-number-visual total-number-visual) obj
       (let ((str1 (if (l-p cur-blk) (format nil "~%~%Letters:  ~S Correct out of ~S" num-correct-letter-visual total-letter-visual) ""))
             (str2 (if (n-p cur-blk) (format nil "~%~%Numbers: ~S Correct out of  ~S" num-correct-number-visual total-number-visual) "")))
         (setf (end-of-blk-msg cnd) (concatenate 'string str1 str2)))
       (setf (%-correct cnd) (calc%correct num-correct-letter-visual num-correct-number-visual total-letter-visual total-number-visual))
       (call-next-method))))

(defmethod end-of-block-msg ((cnd pure-dual-audio) trs cur-blk next-blk)
 (let ((obj (calc-results (make-instance 'nback-results) trs)))
   (with-slots (num-correct-letter-audio total-letter-audio num-correct-number-audio total-number-audio) obj
     (let ((str1 (if (l-p cur-blk) (format nil "~%~%Letters:  ~S Correct out of ~S" num-correct-letter-audio total-letter-audio) ""))
             (str2 (if (n-p cur-blk) (format nil "~%~%Numbers: ~S Correct out of  ~S" num-correct-number-audio total-number-audio) "")))
         (setf (end-of-blk-msg cnd) (concatenate 'string str1 str2)))
      (setf (%-correct cnd)(calc%correct num-correct-letter-audio num-correct-number-audio total-letter-audio total-number-audio))
      (call-next-method))))
                                                                    
(defmethod end-of-block-msg ((cnd mixed-visual-letter) trs  cur-blk next-blk)
 (let ((obj (calc-results (make-instance 'nback-results) trs)))
   (with-slots (num-correct-letter-visual total-letter-visual  num-correct-number-audio total-number-audio) obj
      (setf (end-of-blk-msg cnd) (format nil "~%~%Visual Letters: ~S Correct out of ~S~%~%Audio Numbers  ~S Correct out of ~S" 
                                   num-correct-letter-visual total-letter-visual num-correct-number-audio total-number-audio))
      (setf (%-correct cnd) (calc%correct num-correct-letter-visual num-correct-number-audio total-letter-visual total-number-audio))
      (call-next-method))))

(defmethod end-of-block-msg ((cnd mixed-audio-letter) trs  cur-blk next-blk)
 (let ((obj (calc-results (make-instance 'nback-results) trs)))
   (with-slots ( num-correct-number-visual num-correct-letter-audio  
                total-number-visual total-letter-audio ) obj
      (setf (end-of-blk-msg cnd) (format nil "~%~%Audio Letters: ~S Correct out of ~S~%~%Visual Numbers  ~S Correct out of ~S"
                                     num-correct-letter-audio total-letter-audio  num-correct-number-visual total-number-visual))
      (setf (%-correct cnd) (calc%correct num-correct-letter-audio num-correct-number-visual total-letter-audio total-number-visual))
      (call-next-method))))

(defmethod end-of-block-msg ((cnd pure-nback-visual) trs  cur-blk next-blk)
 (let ((obj (calc-results (make-instance 'nback-results) trs)))
   (with-slots (num-correct-letter-visual total-letter-visual ) obj
     (setf (end-of-blk-msg cnd) (format nil "~%~%Letters:  ~S Correct out of ~S"  num-correct-letter-visual total-letter-visual ))
     (setf (%-correct cnd)(calc%correct num-correct-letter-visual 0 total-letter-visual 0))
     (call-next-method))))



(defun adjust-isi (cur total)
  (when total
    (let* ((last (/ (reduce '+ total) (length total)))
           (diff (- cur last))
           (amt (floor (abs diff) 10)))
     (when (plusp amt)
       (setq +ISI+ (if (plusp diff) (- +ISI+ amt) (+ +ISI+ amt)))
       (if (< +ISI+ +ISI-MIN+) (setq +ISI+ +ISI-MIN+))
       (log-info `(trial new-isi ,+ISI+))))))

(defmethod display-stim (tr (p n-back))
  (with-slots (exp-win  exp-cnd) p
    (case  (type-of exp-cnd)
      ((pure-dual-visual pure-nback-visual)
           (display-stimulus exp-win tr)) 
      (pure-dual-audio 
           (clear-screen exp-win)
           (eeg-event-notify 'stimulus tr)
           (log-info `(trial audio-stimulus :value ,(value tr))) 
           (nback-speak (value tr)) ;;;;(cw-speak (write-to-string (value tr)))
           (display-cross exp-win))
      ((mixed-visual-letter mixed-audio-letter)
       (case (modality tr)
         (visual (display-stimulus exp-win tr))
         (audio (clear-stimulus exp-win) 
                (display-cross exp-win)
                (eeg-event-notify 'stimulus tr)
                (log-info `(trial audio-stimulus :value ,(value tr))) 
                (nback-speak (value tr)) ;;;; (cw-speak (write-to-string (value tr)))   
                ))))))

(defun electrode-message ()
  (capi:prompt-for-string
   "Time to check electrodes. Enter code when done:"
   :initial-value ""
   :value-function  #'(lambda (x)  (values (equal x +passcode+ ) nil))))
         
(defmethod run-exp ((p n-back))
  (with-slots (exp-win current-trials trials exp-cnd in-progress? ) p
    (let ((running% nil))
#+:eeg      (eeg-proc 'begin-record)
      (sleep 3.0)
      (dolist (tr trials)
        (set-mouse-position 0 0)
        (when (and current-trials (> (blk-num tr) (blk-num (first current-trials))))
          (clear-screen exp-win)
          (let ((cur% (end-of-block-msg  exp-cnd current-trials (blk (first current-trials)) (blk tr))))
            (if (and (plusp (blk-num tr)) (zerop (mod (blk-num tr) 4)))
                (while (null (electrode-message)) (capi:beep-pane) (sleep 0.2) (capi:beep-pane)))
            (if +isi-adjustment+ (adjust-isi cur% running%))
            (push cur% running%))
          (setf current-trials nil)
          (sleep 3.0))
        (capi:activate-pane exp-win)
        (push tr current-trials)
        (log-info `(trial start :blk ,(blk-num tr) :trial ,(trial-num tr))) 
        (let ((tm (get-internal-real-time)))
          (setf in-progress? tm)
          (display-stim tr p)
          (sleep 1) ;;;(mp:process-wait-local-with-timeout "nback" 1 #'actual-response tr)
          (clear-screen exp-win)
          (mp:process-wait-local-with-timeout "nback" 1 #'actual-response tr)
          (if (null (actual-response tr)) (log-info `(no-subject-response :expected-respose ,(expected-response tr))))
          (setf in-progress? nil)
          (let* ((tm1 (- (get-internal-real-time) tm))
                 (tm2 (- +ISI+ .200 ( / tm1 1000.0))))
            (display-feedback exp-win tr)
            (if (plusp tm2) (sleep tm2))
            (clear-screen exp-win)
            (sleep .2))))
      (clear-screen exp-win)
#+:eeg      (eeg-proc 'end-record)
      (end-of-block-msg  exp-cnd current-trials (blk (first current-trials)) nil)
      (capi:display-message (format nil "End of Experiment~%~%Thank You!!!" ))
      (nback-done p))))

(defun nback-done (p)
  (if (btn-box p) (disconnect-response-pad))
  (capi:destroy (exp-win p))
  (mp:process-run-function "done" '() #'task-finished (task p)))

(defun run-nback (&key (btn-box nil))
  (let ((p (n-back)) (status nil))
    (setf (btn-box p) btn-box)
    (log-info `(exp-condition ,(type-of (exp-cnd p))))
    (make-blocks p)
    (make-trials p)

    (setf +screen-width+ (capi:screen-width (capi:convert-to-screen)) +screen-height+ (capi:screen-height (capi:convert-to-screen)))
    (setf (exp-win p) (make-instance 'n-back-screen :best-x (- (floor +screen-width+ 2) (floor +window-size+ 2))
                                                    :best-y (- (floor +screen-height+ 2) (floor +window-size+ 2))))
#+:response-pad (when btn-box 
                  (if (null (response-pad-status (get-response-pad))) (setq status  (connect-response-pad)))
                  (if (eql  (response-pad-status (get-response-pad)) 'connected) (enable-response-pad)))
    (log-info `(button-box ,btn-box ,status))
    (capi:display (exp-win p))
    (mp:process-run-function "Nback" '() #' run-exp p)))


(capi:define-interface param-config-window ()
  ()
  (:panes
   (ISI
    capi:text-input-pane
    :title "ISI"
    :text (write-to-string +isi+)
    :title-position :top
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor ISI)
   (num-back
    capi:text-input-pane
    :title "Number Back"
    :text (write-to-string +number-back+)
    :title-position :top
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor num-back)
   (ISI-control capi:check-button 
     :title "ISI Adjustment Enabled" 
     :title-position :top
     :text ""
     :selected +ISI-adjustment+
     :accessor isi-control)
   (random-stims capi:check-button 
     :title "Random Stimulii" 
     :title-position :top
     :text ""
     :selected +random-stimulus+
     :accessor random-stims)
   (num-trials
    capi:text-input-pane
    :title "Number of Real Trials"
    :title-position :top
    :text (write-to-string +num-trials+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor num-trials)
   (num-pract-trials
    capi:text-input-pane
    :title "Number of Practice Trials"
    :title-position :top
    :text (write-to-string +num-practice-trials+)
    :visible-max-width '(character 3)
    :visible-min-width '(character 3)
    :accessor num-pract-trials)
   (num-blks
    capi:text-input-pane
    :title "Number of Blocks"
    :title-position :top
    :text (write-to-string +num-blocks+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor num-blks)
(pract-number-match
    capi:text-input-pane
    :title "Number Match Practice"
    :title-position :top
    :text (write-to-string +num-practice-match+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor pract-number-match)
(number-match
    capi:text-input-pane
    :title "Number Match"
    :title-position :top
    :text (write-to-string +number-match+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor number-match)
#|
   (%-match-number
    capi:text-input-pane
    :title "%-Match-Trials-Number"
    :title-position :top
    :text (write-to-string +%-Match-Trials-Number+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor %-match-number)
   (%-match-letter
    capi:text-input-pane
    :title "%-Match-Trials-Letter"
    :title-position :top
    :text (write-to-string +%-Match-Trials-Letter+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor %-match-letter)
|#
  (match-number
    capi:text-input-pane
    :title "Match Response Number"
    :title-position :top
    :text (format nil "~c" +match-number+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor match-number)
   (match-letter
    capi:text-input-pane
    :title "Match Respone Letter"
    :title-position :top
    :text (format nil "~c" +match-letter+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor match-letter)
  (no-match-number
    capi:text-input-pane
    :title "Different Response Number"
    :title-position :top
    :text (format nil "~c" +no-match-number+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor no-match-number)
   (no-match-letter
    capi:text-input-pane
    :title "Different Respone Letter"
    :title-position :top
    :text (format nil "~c" +no-match-letter+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor no-match-letter)

(lure1-number
    capi:text-input-pane
    :title "Number Lure 1"
    :title-position :top
    :text (write-to-string +num-lure-1+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor lure1-number)

(lure3-number
    capi:text-input-pane
    :title "Number Lure 3"
    :title-position :top
    :text (write-to-string +num-lure-3+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor lure3-number)

(lure1-pract-number
    capi:text-input-pane
    :title "Number Lure 1 Practice"
    :title-position :top
    :text (write-to-string +num-practice-lure-1+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor lure1-pract-number)

(lure3-pract-number
    capi:text-input-pane
    :title "Number Lure 3 Practice"
    :title-position :top
    :text (write-to-string +num-practice-lure-3+)
    :visible-max-width '(character 2)
    :visible-min-width '(character 2)
    :accessor lure3-pract-number)
  
 
   
   (button-done
    capi:push-button
    :text "Done"
    :callback 'param-config-done
    :default-p t)
   )
  (:layouts
   (main capi:column-layout '(info-col button-done) :adjust :center)
   (info-col capi:column-layout '(row-1 row-2 row-3 row-4 row-5 row-6) :adjust :left :accessor info-col)
   (row-1 capi:row-layout '(isi num-back isi-control)) ;; random-stims)) 
   (row-2 capi:row-layout '(num-blks num-pract-trials num-trials))
   (row-3 capi:row-layout '(pract-number-match number-match )) ;;'(%-match-letter %-match-number))
   (row-4 capi:row-layout '(match-letter no-match-letter))
   (row-5 capi:row-layout '(match-number no-match-number))
   (row-6 capi:row-layout '(lure1-pract-number  lure3-pract-number lure1-number lure3-number))
   )
  (:default-initargs
   :title "Parameters"
   :x (- (/ (capi:screen-width (capi:convert-to-screen)) 2) 100)
   :y (- (/ (capi:screen-height (capi:convert-to-screen)) 2) 100))
  )

(defun param-config-done (data win)
  (declare (ignore data))
  (setf +isi+ (ignore-errors (parse-integer (capi:text-input-pane-text (isi win)))))
  (setf +ISI-adjustment+ (capi:button-selected (isi-control win)))
  (setf +number-back+ (ignore-errors (parse-integer (capi:text-input-pane-text (num-back win)))))
  (setf +num-blocks+ (ignore-errors (parse-integer (capi:text-input-pane-text (num-blks win)))))
  (setf +num-trials+ (ignore-errors (parse-integer (capi:text-input-pane-text (num-trials win)))))
  (setf +num-practice-trials+ (ignore-errors (parse-integer (capi:text-input-pane-text (num-pract-trials win)))))
 ; (setf +%-Match-Trials-Letter+ (ignore-errors (parse-integer (capi:text-input-pane-text (%-match-letter win)))))
 ; (setf +%-Match-Trials-Number+ (ignore-errors (parse-integer (capi:text-input-pane-text (%-match-number win)))))
  (setf +num-practice-match+ (ignore-errors (parse-integer  (capi:text-input-pane-text (pract-number-match win)))))
  (setf +Number-Match+ (ignore-errors (parse-integer (capi:text-input-pane-text (number-match win)))))
  (setf +match-letter+ (elt (capi:text-input-pane-text (match-letter win)) 0))
  (setf +no-match-letter+ (elt (capi:text-input-pane-text (no-match-letter win)) 0))
  
  (setf +match-number+ (elt (capi:text-input-pane-text (match-number win)) 0))
  (setf +no-match-number+ (elt (capi:text-input-pane-text (no-match-number win)) 0))
  (setf (num-blocks (n-back)) +num-blocks+)
  (setf (num-trials (n-back)) +num-trials+)
 ; (setf +random-stimulus+ (capi:button-selected (random-stims win)))
  (setf +num-lure-1+ (ignore-errors (parse-integer (capi:text-input-pane-text (lure1-number win)))))
  (setf +num-lure-3+ (ignore-errors (parse-integer (capi:text-input-pane-text (lure3-number win)))))
  (setf +num-practice-lure-1+ (ignore-errors (parse-integer (capi:text-input-pane-text (lure1-pract-number win)))))
  (setf +num-practice-lure-3+ (ignore-errors (parse-integer (capi:text-input-pane-text (lure3-pract-number win)))))
  (capi:exit-dialog nil)
  ;(capi:execute-with-interface win #'(lambda () (capi:destroy win)))
)


#+:cogworld
(let ((p (n-back)))


 (defun n-back-config ()
   (let ((win (make-instance 'param-config-window)))
     (capi:display-dialog win))
   
   (let* ((condition '(Visual-Letter/Visual-Number Audio-Letter/Audio-Number Visual-Letter/Audio-Number Audio-Letter/Visual-Number Traditional))
          (condition-class '(Pure-Dual-Visual Pure-Dual-Audio Mixed-Visual-Letter Mixed-Audio-Letter Pure-NBack-Visual))
          (resp (capi:prompt-with-list condition "Please choose a condition: "))
          )
     (when resp
       ;(define-logging-folder (concatenate 'string (directory-namestring (load-path p)) "N-Back-Data/"))
       ;(setf *cw-debug-mode* t)
       (setf (exp-cnd p) (make-instance (nth (position resp condition) condition-class))) 
       (configuration-done (task p) :condition (nth (position resp condition) '("DV" "DA" "MV" "MA" "NV"))))))

 (defun cw-run-nback ()
   (log-info `(Cogworld))
   (log-info `(parameters +isi+ ,+ISI+ +Number-Back+ ,+Number-Back+ +ISI-Adjustment+ ,+ISI-Adjustment+
                         ; +%-Match-Trials-Letter+ ,+%-Match-Trials-Letter+ +%-Match-Trials-Number+ ,+%-Match-Trials-Number+ 
                          +Number-Match+ ,+Number-Match+ +num-practice-match+ ,+num-practice-match+
                          +num-blocks+ ,+num-blocks+ +num-trials+ ,+num-trials+
                          +match-letter+ ,+match-letter+ +no-match-letter+ ,+no-match-letter+ +match-number+ ,+match-number+
                          +no-match-number+ ,+no-match-number+
                          +num-lure-1+ ,+num-lure-1+ +num-lure-3+ ,+num-lure-3+
                          +num-practice-lure-1+ ,+num-practice-lure-1+ +num-practice-lure-3+ ,+num-practice-lure-1+))
   (let ((btn-box (capi:button-selected (check-response-pad (control-window *cw*)))))
      
     (run-nback :btn-box btn-box) ))

(setf (task p) (register-task "NBACK" :run-function #'cw-run-nback :configure-function #'n-back-config))
)



(defun play-sound-file (str)
  (objc:with-autorelease-pool ()
    (let ((url (objc:invoke "NSURL"  "fileURLWithPath:" str)))
      ;; Trying to execute [[[NSSound alloc] initWithContentsOfFile:url FALSE] play]
      ;; to play file at url (is only a fragment of what actually needs to be called, assuming LispWorks
      ;; takes care of it for us)
      (objc:invoke (objc:invoke (objc:invoke "NSSound" "alloc")
                                "initWithContentsOfURL:byReference:"
                                url nil) "play"))))

(let* ((load-path (current-pathname))
       (file-path (concatenate 'string (directory-namestring load-path) "sounds/~S.wav")))

(defun nback-speak (item)
  (play-sound-file (format nil file-path item))))


(defun test-tm (s)
  
  (dolist (str (mapcar 'write-to-string s)) (let ((tm (get-internal-real-time))) (cw-speak str) (format t "~%~S ~S" str (- (get-internal-real-time) tm)))))

(defvar results nil)
(defun play-sound-files ()
  (dolist (item '(0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z))
    (let ((fn (format nil "/Projects/nback/sounds/~S.wav" item))
          (tm (get-internal-real-time)))
      (play-sound-file fn)
      (push (list item (- (get-internal-real-time) tm)) results) 
      (sleep 1))))
 


