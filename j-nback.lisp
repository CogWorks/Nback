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

(defmacro conv-to-list (x)
  `(if ,x (ignore-errors (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) ,x) ")")))))

(defun read-input (file-name)
 (let ((inp t) (accum nil)  (cnt 0))
   (with-open-file (fs (namestring file-name)  :direction :input)
      (while inp
        (setq inp  (ignore-errors (read-line fs nil)))
        (incf cnt)
        (when inp 
          ;(print inp)
          (setq inp (conv-to-list inp))
          (push inp accum))))
   (reverse accum)))



(defun permute-list (lst)
  (let* ((res nil) (accum nil)
         (index-list (dotimes (i (length lst) res) (push i res))))
    (dotimes (i (length lst) accum)
      (let ((idx (nth (random (length index-list)) index-list)))
        (push (nth idx lst) accum)
        (setf index-list (remove idx index-list))))))

(defun mean (lst)
  (let ((l (remove nil lst)))
    (if (null l) 0 (/ (reduce '+ l) (length l)))))
(defparameter +strategy+ nil)
(defparameter +stimulii+ nil)       
(defparameter +num-blocks+ 12)
(defconstant +passcode+ "cogworks")
(defconstant +window-size+ 800)
(defconstant +letter-size+ 34)
(defparameter +screen-width+ (capi:screen-width (capi:convert-to-screen)))
(defparameter +screen-height+ (capi:screen-height (capi:convert-to-screen)))
(defconstant +cross-size+ 34)
(defparameter +match-letter+ #\a)
(defparameter +no-match-letter+ #\d)
(defparameter +practice-trials+ '(((R x 0) (J x 0) (Z x 0) (H DIST 0) (M DIST 0) (Z MATCH 3) (H MATCH 3) (M MATCH 3) (Z MATCH 3) (H MATCH 3) (B DIST 0) (X DIST 0) (B N-1 2) (J DIST 12) (X MATCH 3) (R DIST 15) (Z DIST 8) (X MATCH 3) (F DIST 0) (R N+1 4) (X MATCH 3) (F MATCH 3) (R MATCH 3) (Z DIST 7) (F MATCH 3) (R MATCH 3) (M DIST 19) (R N-1 2) (B DIST 16) (X DIST 9) (F DIST 6) (B MATCH 3) (X MATCH 3) (J DIST 20) (B MATCH 3))

((H x 0) (J x 0) (Z x 0) (H MATCH 3) (R DIST 0) (F DIST 0) (R N-1 2) (J DIST 6) (F MATCH 3) (R MATCH 3) (J MATCH 3) (F MATCH 3) (R MATCH 3) (H DIST 10) (M DIST 0) (R MATCH 3) (Z DIST 14) (Z DIST 15) (F DIST 7) (X DIST 0) (B DIST 0) (M DIST 7) (H DIST 9) (B MATCH 3) (M MATCH 3) (H MATCH 3) (B MATCH 3) (R DIST 12) (H MATCH 3) (F DIST 11) (R MATCH 3) (H MATCH 3) (M DIST 8) (X DIST 14) (H MATCH 3) )

((R x 0) (B x 0) (Z x 0) (R MATCH 3) (B MATCH 3) (F DIST 0) (J DIST 0) (R N+1 4) (F MATCH 3) (R N-1 2) (M DIST 0) (M DIST 0) (F N+1 4) (X DIST 0) (J DIST 8) (Z DIST 13) (X MATCH 3) (J MATCH 3) (B DIST 14) (X MATCH 3) (B N-1 2) (F DIST 9) (X MATCH 3) (M DIST 12) (H DIST 0) (Z DIST 10) (M MATCH 3) (X DIST 5) (Z MATCH 3) (M MATCH 3) (X MATCH 3) (B DIST 11) (M MATCH 3) (B N-1 2) (H DIST 10))

((Z x 0) (M x 0) (X x 0) (F DIST 0) (H DIST 0) (Z DIST 5) (F MATCH 3) (B DIST 0) (R DIST 0) (M DIST 8) (X DIST 8) (R MATCH 3) (M MATCH 3) (J DIST 0) (H DIST 10) (X DIST 5) (J MATCH 3) (X N-1 2) (H N+1 4) (J MATCH 3) (X MATCH 3) (H MATCH 3) (R DIST 11) (B DIST 16) (H MATCH 3) (M DIST 13) (Z DIST 21) (B N+1 4) (Z N-1 2) (X DIST 9) (B MATCH 3) (Z MATCH 3) (F DIST 26) (X N+1 4) (Z MATCH 3)))) 


(defclassic N-back-block ()
 num
 trials)


(defclassic practice-block (N-back-block)
  (ISI 3.000))

(defclassic short-block (N-back-block)
  (ISI 3.000))
(defclassic long-block (N-back-block)
  (ISI 3.000))

(defclassic n-back-trial ()
 blk
 blk-num 
 trial-num
 stimulus
 modality
 value
 trial-type
 num-since
 expected-response
 response-time 
 actual-response)
   
(defclassic n-back ()
  (num-blocks +num-blocks+)
  blocks
  exp-win
  task
  (exp-name 'Nback)
  strategy-sel-win 
  (load-path (current-pathname))
  (stimulus-display-time 1.0)
  current-trial
  in-progress? 
  btn-box
  )

(defclassic n-back2 (n-back)
  (block-order '(psl  sl_s sl_l sl_s sl_l sl_s sl_l sl_s sl_l sl_s sl_l sl_s sl_l))
  (stimulus-display-time 1.5))

(defclassic n-back3 (n-back2)
  (stimulus-display-time 2.0))

(let ((n-back (make-instance 'n-back3 )))
  (defun n-back () n-back))

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

(defmethod make-blocks ((p n-back2))
  (let ((n (round (/  (num-blocks p) 2)))
        (seq1 '(0 1 2 3))
        (seq (permute-list '(4 5 6 7 8 9 10 11 12 13 14 15)))) 
    
    (setq seq (append seq1 seq))
    (log-info `(block-sequence ,seq))
    (dotimes (i n)
      (push (make-instance 'long-block) (blocks p))
      (push (make-instance 'short-block) (blocks p)))
    (setf (blocks p) (permute-list (blocks p)))
    (dotimes (i 4)
      (push (make-instance 'practice-block) (blocks p) ))
    (setf +stimulii+ (append +practice-trials+  +stimulii+))
    
    (dotimes (i (length (blocks p)))
      (dotimes (j (length (nth i +stimulii+)))
        (destructuring-bind (stim typ back) (nth j (nth (nth i seq)  +stimulii+))
          (setf (num (nth i (blocks p) )) i) 
          (push  (make-instance 'n-back-trial :value stim :trial-type typ :num-since back :stimulus 'L :trial-num j :modality 'visual
                                :blk (nth i (blocks p) )  
                                :expected-response (if (eql typ 'x) nil (if (eql 'match typ) 'match 'no-match)))
                 (trials (nth i (blocks p) )))))
        (setf (trials (nth i (blocks p) )) (reverse (trials (nth i (blocks p) )) )))))

(defmethod make-blocks ((p n-back3))
  (let* ((seq '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (num-blocks (length seq))
         (r-lst nil)) 
    (dotimes (k 1000)
      (push k r-lst) )
    (setf r-lst (permute-list r-lst))
    (log-info `(block-sequence ,(subseq r-lst 0 num-blocks)))
    (dotimes (i num-blocks)
      (push (make-instance 'long-block) (blocks p)))
    (dotimes (i num-blocks)
      (dotimes (j (length (nth i +stimulii+)))
        (if (or (and (< i 4) (< j 32)) (>= i 4))
            (destructuring-bind (stim typ back) (nth j (nth (nth i r-lst)  +stimulii+))
              (setf (num (nth i (blocks p) )) i) 
              (push  (make-instance 'n-back-trial :value stim :trial-type typ :num-since back :stimulus 'L :trial-num j :modality 'visual
                                    :blk (nth i (blocks p) )  
                                    :expected-response (if (eql typ 'x) nil (if (eql 'match typ) 'match 'no-match)))
                     (trials (nth i (blocks p) ))))))
      (setf (trials (nth i (blocks p) )) (reverse (trials (nth i (blocks p) )) )))))
    
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

(defparameter txt1 "Please rate your ability to use the instructed strategy on a scale of 1 to 7")
(defparameter txt2 "1 meaning that you were unable to use the strategy, and 7 meaning that you were able to use the strategy consistently.")

(defparameter txt3 "If you had difficulty using the correct strategy and would like some help, open the door and ask the experimenter for assistance.")

(capi:define-interface likert-screen ()
  ((sel :initform nil :accessor sel))
  (:panes
   (likert capi:radio-button-panel :items '(1 2 3 4 5 6 7) :accessor likert :default-button nil :initial-focus-item nil
           :keep-selection-p nil :accepts-focus-p nil
           :selection-callback  (lambda(data win) (setf (sel win) data) )))
  (:layouts
   (strategy-selection capi:column-layout `(,txt1 ,txt2 likert ,txt3) :accessor strategy-selection))
  (:default-initargs
   :window-styles '(:borderless :toolbox)
   :layout 'strategy-selection
   :display-state :hidden))
   

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
   (strategy-selection capi:column-layout `(,txt1 ,txt2 likert ,txt3) :accessor strategy-selection)
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

(defmethod display-feedback3 ((interface n-back-screen) tr)
    (let ((sq (make-instance 'square :filled t :foreground :black )))
      (capi:apply-in-pane-process (screen interface) (lambda(win) (setf (capi:layout-description (screen win)) (list sq))) interface)))



(defmethod calc-actual-response ((tr n-back-trial) key)
  (cond ((and  (eql (stimulus tr) 'L) (eql (char-downcase key) +match-letter+) 'match))
        ((and  (eql (stimulus tr) 'L) (eql (char-downcase key) +no-match-letter+) 'no-match))
        (t  (capi:beep-pane) (sleep .1) (capi:beep-pane)
            'invalid-response))) ;

     
(defun char-input-callback (self x y key)
  (declare (ignore x y))

  (let ((tr (current-trial (n-back))))
       
       (cond ((null (in-progress? (n-back)))
              (log-info `(subject-input :response ,key :trial-num ,(trial-num tr) :stimulus ,(value tr) :serial-position ,(num-since tr) :trial-type ,(trial-type tr)  
                                        :expected-response ,(expected-response tr) 
                                        :actual-response  ,(if (actual-response tr) 'already-responded (setf (actual-response tr) 'too-late)))))
             ((and (expected-response tr)
                   (or (eql key +match-letter+) (eql key +no-match-letter+)  ))
              (setf (response-time tr) (- (get-internal-real-time) (in-progress? (n-back))) )
              (log-info `(subject-input :response ,key :trial-num ,(trial-num tr) :stimulus ,(value tr)  :serial-position ,(num-since tr) :trial-type ,(trial-type tr) 
                                        :expected-response ,(expected-response tr)  :response-time ,(- (get-internal-real-time) (in-progress? (n-back)))
                                        :actual-response  ,(if (actual-response tr) 'already-responded (setf (actual-response tr)  (calc-actual-response tr key))))))
             (t
              (setf (actual-response tr) 'invalid)
              (log-info `(subject-input :response ,key :trial-num ,(trial-num tr) :stimulus ,(value tr) :serial-position ,(num-since tr) :trial-type ,(trial-type tr) 
                                        :expected-response ,(expected-response tr) :response-time ,(- (get-internal-real-time) (in-progress? (n-back))) 
                                        :actual-response invalid-response ))
              (capi:beep-pane self) (sleep .1) (capi:beep-pane self)))
       (if (eql (actual-response tr) 'match) (eeg-event-notify 'response tr) (eeg-event-notify 'error tr))
       ))

;;;
;;;buton box processing
;;;
(defun process-response (btn ts)
  (declare (ignore ts)) 
  (with-slots ( in-progress? current-trial ) (n-back)
    (let ((tr current-trial))
      (when tr
        
        (cond ((and (expected-response tr) (numberp btn) (<= 3 btn 4) (numberp in-progress?) (plusp in-progress?))
               (setf (response-time tr) (- (get-internal-real-time) in-progress?) )
               (log-info `(subj-action :event button-press :response-time ,(response-time tr) :response,btn  ))
               
               (log-info `(subj-input :response ,btn :trial-num ,(trial-num tr) :stimulus ,(value tr) :serial-position ,(num-since tr) :trial-type ,(trial-type tr) 
                                      :expected-response ,(expected-response tr) :response-time ,(- (get-internal-real-time) (in-progress? (n-back)))
                                      :actual-response ,(if (actual-response tr) 'already-responded 
                                                          (setf (actual-response tr) (cond ((or (= btn 1) (= btn 3)) 'match)
                                                                                           ((or (= btn 2) (= btn 4)) 'no-match)
                                                                                           (t 'invalid-response)))))))   
              ((null in-progress?)
               (log-info `(subject-input :response ,btn :trial-num ,(trial-num tr) :stimulus ,(value tr) :serial-position ,(num-since tr) :trial-type ,(trial-type tr)
                                        :expected-response ,(expected-response tr) 
                                        :actual-response  ,(if (actual-response tr) 'already-responded (setf (actual-response tr) 'too-late)))))              
              (t
               (setf (actual-response tr) 'invalid)
               (log-info `(subj-input :response ,btn :trial-num ,(trial-num tr) :stimulus ,(stimulus tr) :serial-position ,(num-since tr) :trial-type ,(trial-type tr)  
                                      :expected-response ,(expected-response tr) :response-time ,(- (get-internal-real-time) (in-progress? (n-back)))
                                      :actual-response invalid-response))))
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
                                (log-info `(trial display-stimulus :value ,(value tr) :serial-position ,(num-since tr)))) 
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

(defun electrode-message ()
  (capi:prompt-for-string
   "Time to check electrodes. Enter code when done:"
   :initial-value ""
   :value-function  #'(lambda (x)  (values (equal x +passcode+ ) nil))))

(defun chk-electrodes(n)
  (if (and (plusp n) (zerop (mod n 4))) 
      (while (null (electrode-message)) (capi:beep-pane) (sleep 0.2) (capi:beep-pane))))
         

(defmethod run-exp ((p n-back2))
  (with-slots (exp-win in-progress? stimulus-display-time blocks current-trial ) p
    
#+:eeg      (eeg-proc 'begin-record)
      (dotimes (i (length blocks))
        (let ((blk (nth i blocks)) (continue? t))
          (if (eql i 3) (setq continue? (capi:prompt-for-confirmation  "Another Practice Block?")))
          (when continue?
            (set-mouse-position 0 0)
            (clear-screen exp-win)
            ;;#+:eeg      (chk-electrodes i)        
            (sleep .5)
            (capi:activate-pane exp-win)
            (dotimes (j (length (trials blk))) 
              (let ((tr (nth j (trials blk))))
                (setf current-trial tr)
                (log-info `(trial start  :blk ,i :trial ,(trial-num tr))) 
                (let ((tm (get-internal-real-time)))
                  (setf in-progress? tm)
                  (display-stimulus exp-win tr)
                  
                  (cond ((and (eql i 0) (expected-response tr))
                         (mp:process-wait-with-timeout "self paced" 120 'actual-response tr ))
                        (t
                         (sleep stimulus-display-time)))
                  (clear-screen exp-win)
                  (setf in-progress? nil)
                  (if (and (null (actual-response tr)) (expected-response tr)) (log-info `(no-subject-response :expected-respose ,(expected-response tr))))
                  (if (< i 2)
                      (display-feedback exp-win tr)
                    (if (and (typep p 'n-back3) (null (actual-response tr)) (expected-response tr))
                        (display-feedback3 exp-win tr)))
                  (sleep (isi blk)) 
                  (clear-screen exp-win)
                  )))
            #+:eeg    (eeg-proc 'end-record)
            (end-of-block-msg blk p) )))
      (capi:display-message (format nil "End of Experiment~%~%Thank You!!!" ))
      (nback-done p)))

(defun chop (n)
  (/ (round (* 100 n)) 100.00))

(defmethod end-of-block-msg (blk (p n-back2))
  (let ((rt (chop (/ (mean (mapcar 'response-time (trials blk))) 1000.0)))
        (acc (round (* 100.0 (mean (mapcar (lambda(obj) (if (eql (expected-response obj) (actual-response obj)) 1 0)) 
                                           (remove-if-not (lambda(y) (eql (trial-type y) 'match)) (subseq (trials blk) 3)))))))
        (acc-all (round (* 100.0 (mean (mapcar (lambda(obj) (if (eql (expected-response obj) (actual-response obj)) 1 0)) 
                                            (subseq (trials blk) 3))))))
        (win (exp-win p)))
    (log-info `(block-feedback :block ,(num blk) :accuracy ,acc :rt ,rt :accuracy-all ,acc-all))
    ;(log-info `(debug ,(mapcar (lambda(obj) (if (eql (expected-response obj) (actual-response obj)) 1 0)) (trials blk)) ,(mean (mapcar 'response-time (trials blk)))))
    (cond ((< (num blk) 2)
          (capi:display-message (format nil "End of Block ~S ~%~%Your accuracy for targets was ~S %   ~%~%Your total accuracy was ~S %~%~% Your response time was ~S seconds" (num blk) acc acc-all rt)))
          (t 
           (if (equal +strategy+ "no-training")
               (capi:display-message (format nil "End of Block ~S ~%~% Click OK for next Block" (num blk)))
             (let ((win2 (strategy-sel-win p)))
               (capi:hide-interface win nil)
               (setf (sel win2) nil)
               (capi:show-interface win2)
               (capi:activate-pane win2)
               (setf (capi:choice-selection (likert win2)) nil)
               (mp:process-wait-with-timeout "selecting" 600 'sel win2 )
               (log-info `(strategy-selection :choice ,(sel win2)))
               (capi:hide-interface win2 nil)
               (capi:show-interface win)))))))
           

(defun nback-done (p)
  (if (btn-box p) (disconnect-response-pad))
  (capi:destroy (exp-win p))
  (mp:process-run-function "done" '() #'task-finished (task p)))

(defun run-nback (&key (btn-box nil))
  (let ((p (n-back)) (status nil))
    (setf (btn-box p) btn-box)
    (log-info `(Strategy ,+strategy+))
    (make-blocks p)
    

    (setf +screen-width+ (capi:screen-width (capi:convert-to-screen)) +screen-height+ (capi:screen-height (capi:convert-to-screen)))
    (setf (exp-win p) (make-instance 'n-back-screen :best-x (- (floor +screen-width+ 2) (floor +window-size+ 2))
                                                    :best-y (- (floor +screen-height+ 2) (floor +window-size+ 2))))
    (setf (strategy-sel-win p) (make-instance 'likert-screen :best-x (- (floor +screen-width+ 2) (floor +window-size+ 2))
                                                             :best-y 20))
#+:response-pad (when btn-box 
                  (if (null (response-pad-status (get-response-pad))) (setq status  (connect-response-pad)))
                  (if (eql  (response-pad-status (get-response-pad)) 'connected) (enable-response-pad)))
    (log-info `(button-box ,btn-box ,status))
    (capi:display (strategy-sel-win p))
    (capi:display (exp-win p))

    (mp:process-run-function "Nback" '() #' run-exp p)))



#+:cogworld
(let ((p (n-back))
      (pn (directory-namestring (current-pathname)))
      )

(defun n-back-config ()
  (let* ((condition '("even_24" "skewed_24"))
         (resp (capi:prompt-with-list condition "Please choose a condition: "))
         (files (mapcar 'file-namestring (directory (concatenate'string pn "nback_lists/"))))
         (fn (find resp files :key (lambda(y) (subseq y 0 (length resp))) :test 'equal)))
    (when fn
      (setf +stimulii+ (read-input (concatenate'string pn "nback_lists/" fn)))
      (let* ((strategy '("rolling" "static" "no-training"))
             (resp2 (capi:prompt-with-list strategy "Please choose a condition: ")))
        (when resp2
          (setf +strategy+ resp2)
        
          (configuration-done (task p) :condition resp))))))
      
 (defun cw-run-nback ()
   (log-info `(Cogworld ))
   (let ((btn-box (capi:button-selected (check-response-pad (control-window *cw*)))))
     (run-nback :btn-box btn-box) ))

(setf (task p) (register-task "NBACK" :run-function #'cw-run-nback  :configure-function #'n-back-config ))
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
 


