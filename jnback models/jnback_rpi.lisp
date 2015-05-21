
#|

To run: (do-nback :num n :fn path :who 'model :mode 'audio ) (run-time 3) (lf nil)
where n = number of trials to do or 'all for all trials in seq file (defaults to 'all)
fn = path to seq file, if not given, you will be prompted for a file
who = model or human, defaults to 'model
mode = visual or audio, defaults to audio
run-time is number of seconds between stimulii
lf = t if want a log file created the nameof the logfile will be the name of the sequence file concatenated with
     yearmonthdday_hourminutesecond (2 char each)

Note: if you want to hear the stimulus uncomment the ;(lw-speak stimulus  :voice 3) line
(may affect timing)

|#
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro conv-to-list (x)
  `(if ,x (ignore-errors (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) ,x) ")")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-nback (&key (num 'all) (fn (capi:prompt-for-file "")) (who 'model) (mode 'audio) (run-time 3) (lf t))
  (let ((data (read-input fn))
        (log-file-name (if lf (make-log-file-name (namestring fn)))))
    (if lf (write-file-header log-file-name))
    (if (eql num 'all) (setq num (length data)))
    (dotimes (i num)
      (let* ((tr (nth i data))
             (results (jnback tr :who who :mode  mode :run-time run-time)))
        (if log-file-name (write-results log-file-name results i)))
        )))


(defun make-log-file-name (fn)
  (multiple-value-bind (second minute hour day month year daylight zone other) (get-decoded-time)
    (declare (ignore  daylight zone other))
      (format nil "~A-~2,'0D~2,'0D~2,'0D_~2,'0D~2,'0D~2,'0D.txt"
                         fn
                         (mod year 2000)
                         month
                         day
                         hour
                         minute
                         second)))

(defun write-file-header (fn)
  (with-open-file (fs fn :direction :output :if-exists :append :if-does-not-exist :create)
    (write 'seq_num :stream fs) (write-char #\tab fs)
    (write 'stimulus  :stream fs) (write-char #\tab fs)
    (write 'type :stream fs) (write-char #\tab fs)
    (write 'ser_pos :stream fs) (write-char #\tab fs)
    (write 'stimulus_time :stream fs) (write-char #\tab fs)
    (write 'response_time :stream fs) (write-char #\tab fs)
    (write 'response :stream fs) (write-char #\newline fs))) 

(defun write-results (fn log n)
  (with-open-file (fs fn :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (lst log)
      (write n :stream fs) (write-char #\tab fs)
      (destructuring-bind (stim tr-type ser-pos stim-tm response-tm response) lst
        (write stim  :stream fs) (write-char #\tab fs)
        (write tr-type :stream fs) (write-char #\tab fs)
        (write ser-pos :stream fs) (write-char #\tab fs)
        (write stim-tm :stream fs) (write-char #\tab fs)
        (write response-tm :stream fs) (write-char #\tab fs)
        (write response :stream fs) (write-char #\newline fs)) 
      )))



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

(defvar *response* nil)

(defparameter *m* nil)
(defun visual? ()
  (eql *m* 'visual))

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (list (mp-time) (string key) ))
  (format t "~%RESPONSE ~S" *response* )
  (when (visual?) 
    (clear-exp-window)
    (proc-display)))

(defun jnback (trial-seq &key who mode run-time)
  (reset)
  (setf *m* mode)
  (let ((model (if (eq who 'human) nil t))
        (stimulus nil)
        (stimulus-time nil)
        (accum nil)
        (window (open-exp-window "N-Back" :visible (visual?))))
    (install-device window)
    (dolist (tr trial-seq)
      (setf *response* '(NA NA)) 
      (setf stimulus (write-to-string (first tr)))
      (cond (model
             (cond ((visual?) 
                    (add-text-to-exp-window :text stimulus :x 125 :y 150)
                    (proc-display))
                   (t ;audio
                    ;(lw-speak stimulus  :voice 3)
                    (new-word-sound stimulus)))
             (setf stimulus-time (mp-time))
             (run-full-time run-time :real-time nil)
             (when (and (visual?) (equal *response* '(NA NA)))
               (clear-exp-window)
               (proc-display))
             (push (concatenate 'list tr (list stimulus-time) *response*) accum) )
            (t
             (cond ((visual?) 
                    (add-text-to-exp-window :text stimulus :x 125 :y 150))
                   (t
                    (lw-speak stimulus  :voice 3)))
             (sleep run-time)
             (push (concatenate 'list tr (list stimulus-time) *response*) accum))))
    (reverse accum)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(clear-all)

(define-model jnback

(sgp :seed (123456 0))
(sgp :v t :needs-mouse nil  :trace-detail low :bll .5 :rt 1 :act t :blc 1 :esc t)
  
(chunk-type letters state three)
(chunk-type item letter)
(chunk-type array letter1 letter2 letter3 recently-retrieved)

(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk)
 (goal isa letters state start three no))

(P find-unattended-letter
   =goal>
      ISA         letters
      state       start
 ==>
   +aural-location>
      ISA         audio-event
      :attended    nil
   =goal>
      state       find-location
)

(P attend-letter
   =goal>
      ISA         letters
      state       find-location
   =aural-location>
      ISA         audio-event
   
   ?aural>
      state       free
   
==>
   +aural>
      ISA         sound
      event       =aural-location
   =goal>
      state       attend
)

(P encode-first-letter
   =goal>
      ISA         letters
      state       attend
   =aural>
      ISA         sound
      content       =letter

   ?imaginal>
      state       free 
==>

   =goal>
   state     start
  
   +imaginal>
      isa         array
      letter1      =letter
)

(p encode-second-letter
   
    =goal>
      ISA         letters
      state       attend
   =aural>
      ISA         sound
      content       =letter

   =imaginal>
      ISA        array
      letter2    nil

==>
    =goal>
   state     start

   =imaginal>
      letter2   =letter
)

(p encode-third-letter
   
    =goal>
      ISA         letters
      state       attend
   =aural>
      ISA         sound
      content       =letter

   =imaginal>
      ISA        array
      letter2    =x
      letter3    nil

==>
    =goal>
   state     start
   three     yes

   =imaginal>
      letter3   =letter
)

(p hold-item-and-retrieve-list

   =goal>
      ISA         letters
      state       attend
      three       yes

   =aural>
      ISA         sound
      content       =letter

   ?imaginal>
    state free
    

   ?retrieval>
      state    free

==>

   =goal>
      state    retrieve
   +imaginal>
      ISA  item
      letter   =letter
   +retrieval>
      ISA array
     
)

(p detect-match

   =goal>
      ISA         letters
      state       retrieve
   =retrieval>
      ISA          array
      letter1       =letter
      letter2      =b
      letter3      =c
      

   =imaginal>
      ISA        item
      letter     =letter

   ?manual>
       state     free

==>

   =goal>
      state   harvest

   +manual>
     ISA    press-key
     key    "m"

   +imaginal>
      ISA        array
      letter1   =b
      letter2   =c
      letter3   =letter

   -retrieval>
  

)

(p detect-no-match

   =goal>
      ISA         letters
      state       retrieve

   =imaginal>
      ISA        item
      letter     =letter

   =retrieval>
      ISA          array
      - letter1      =letter
      letter2      =b
      letter3      =c

   

   ?manual>
       state     free

==>

   =goal>
      state   harvest

   +manual>
     ISA    press-key
     key    "x"

   +imaginal>
      ISA        array
      letter1   =b
      letter2   =c
      letter3   =letter

   -retrieval>

)

(p harvest-imaginal

   =goal>
      ISA letters
      state harvest

==>

 =goal>
  state start
-imaginal>
)


(goal-focus goal)

)

