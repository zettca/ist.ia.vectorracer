(load "SolF3.lisp")

(defun states-to-poss (stts)
  (loop for st in stts
	  collect (state-pos st)))

(defun states-to-list (stts)
  (loop for st in stts
	  collect (format t "POS: ~a VEL: ~a ACT: ~a COST: ~a~&"
	  (state-pos st)  (state-vel st)  (state-action st)  (state-cost st))))

(defun initial-state (track)
  (make-state :pos (track-startpos track) :vel (make-vel 0 0) :action nil :cost 0 :track track))

(defun printtrack (track)
    (loop for line in (track-env track) do
        (print (substitute 'X nil line))))

(defun testTrack (trackname)
    ;(format t "~&========== ~a ==========~&" trackname)
    (setf *t* (loadtrack trackname))
    (setf *s* (initial-state *t*))
    (setf *p* (make-problem :initial-state (initial-state *t*) :fn-isGoal #'isGoalp :fn-nextstates #'nextStates :fn-h #'compute-heuristic))
    (let ((time1 (get-internal-real-time)) (time2) (time3) (time4) (resA) (resB))
        (format t "~&Calculating Heuristic   ")
        (compute-heuristic *s*)
        (setf time2 (get-internal-real-time))
        (format t "[~f seconds]~%" (/ (- time2 time1) internal-time-units-per-second))
        (format t "~&Calculating best-search ")
        (setf resB (best-search *p*))
        (setf time3 (get-internal-real-time))
        (format t "[~f seconds]~%" (/ (- time3 time2) internal-time-units-per-second))
        (format t "~&Calculating A* search   ")
        (setf resA (a* *p*))
        (setf time4 (get-internal-real-time))
        (format t "[~f seconds]~%" (/ (- time4 time3) internal-time-units-per-second))
        ;(states-to-list resA)
        ))

(defvar *t* nil)
(defvar *s* nil)
(defvar *p* nil)

; EASY TRACKS
(testTrack "track0.txt")
(testTrack "track9.txt")
; MEDIUM TRACKS
(testTrack "track20.txt")
(testTrack "track22.txt")
(testTrack "track24.txt")
(testTrack "track26.txt")
; HARD TRACKS
(testTrack "track32.txt")
(testTrack "track34.txt")
