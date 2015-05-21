
#|

To run: (do-back :num n :fn path :who 'model :mode 'audio ) (run-time 3) (lf nil)
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

(defun do-nback (&key (num 'all) (fn (capi:prompt-for-file "")) (who 'model) (mode 'audio) (run-time 3) (lf nil))
  (let ((data (read-input fn))
        (log-file-name (if lf (make-log-file-name (namestring fn)))))
    (if (eql num 'all) (setq num (length data)))
    (dotimes (i num)
      (let* ((tr (nth i data))
             (results (jnback tr :who who :mode  mode :run-time run-time)))
        (if log-file-name (write-log-file log-file-name results)))
        )))


(defun make-log-file-name (fn)
  (multiple-value-bind (second minute hour day month year daylight zone other) (get-decoded-time)
    (declare (ignore  daylight zone other))
      (format nil "~A-~2,'0D~2,'0D~2,'0D_~2,'0D~2,'0D~2,'0D"
                         fn
                         (mod year 2000)
                         month
                         day
                         hour
                         minute
                         second)))

(defun write-log-file (fn log)
  (with-open-file (fs fn :direction :output :if-exists :append :if-does-not-exist :create)
     (write log :stream fs)
     (write-char #\newline fs)))



(defun read-input (file-name)
 (let ((inp t) (accum nil)  (cnt 0))
   (with-open-file (fs (namestring file-name)  :direction :input)
      (while inp
        (setq inp  (ignore-errors (read-line fs nil)))
        (incf cnt)
        (when inp 
          (print inp)
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
             (run-full-time run-time :real-time t)
             (when (and (visual?) (equal *response* '(NA NA)))
               (clear-exp-window)
               (proc-display))
             (push (append tr *response*) accum))
            (t
             (cond ((visual?) 
                    (add-text-to-exp-window :text stimulus :x 125 :y 150))
                   (t
                    (lw-speak stimulus  :voice 3)))
             (sleep run-time)
             (push (concatencate 'list tr (list stimulus-time) *response*) accum))))
    (reverse accum)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(clear-all)

(define-model jnback

(sgp :seed (123456 0))
(sgp :v t :needs-mouse nil  :trace-detail low :bll .5 :rt 1 :act t :blc 1 :esc t :ans .1)
  
(chunk-type letters state three)
(chunk-type item letter)
(chunk-type array letter1 letter2 letter3 recently-retrieved)
(chunk-type item letter)
(chunk-type hold letter)

(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk)
  (letter-a isa item letter "A")
 (letter-b isa item letter "B")
 (letter-c isa item letter "C")
 (letter-d isa item letter "D")
 (letter-e isa item letter "E")
 (letter-f isa item letter "F")
 (letter-g isa item letter "G")
 (letter-h isa item letter "H")
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



(p retrieve-target

   =goal>
      ISA letters
      state attend
     
   ?retrieval>
      state free
   =aural>
      ISA sound
      content =letter
   
==>

   =goal>
      state retrieve
   +retrieval>
      isa item
      letter =letter
   +imaginal>
      isa hold
      letter =letter
      
)

(p respond-yes

   =goal>
   ISA letters
   state retrieve

   =retrieval>
   ISA item
   letter =letter

   =imaginal>
   ISA hold
   letter =letter

    ?manual>
       state     free

==>

   =goal>
   state start

    +manual>
     ISA    press-key
     key    "m"
)

(p respond-no

   =goal>
   ISA letters
   state retrieve

   ?retrieval>
   state error

   =imaginal>
   ISA hold
   letter =letter

   ?manual>
   state free

==>

   =goal>
   state start

   +manual>
     ISA    press-key
     key    "x"
   
   +imaginal>
     ISA item
     letter =letter
     
   

)

(goal-focus goal)

)

