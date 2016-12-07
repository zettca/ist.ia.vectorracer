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

(defvar *t* nil)
(defvar *s* nil)
(defvar *p1* nil)

(setf *t* (loadtrack "track21.txt"))
(setf *s* (initial-state *t*))
(setf *p* (make-problem :initial-state (initial-state *t*) :fn-isGoal #'isGoalp :fn-nextstates #'nextStates :fn-h #'compute-heuristic))

(let ((real1 (get-internal-real-time)))
    (format t "~&Track 21 - Heuristic~&")
    (compute-heuristic *s*)
    (format t "~&Track 21 - A*~&")
    (states-to-list (a* *p*))
    (let ((real2 (get-internal-real-time)))
    (format t "Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))


(setf *t* (loadtrack "track22.txt"))
(setf *s* (initial-state *t*))
(setf *p* (make-problem :initial-state (initial-state *t*) :fn-isGoal #'isGoalp :fn-nextstates #'nextStates :fn-h #'compute-heuristic))

(let ((real1 (get-internal-real-time)))
    (format t "~&Track 22 - Heuristic~&")
    (compute-heuristic *s*)
    (format t "~&Track 22 - A*~&")
    (states-to-list (a* *p*))
    (let ((real2 (get-internal-real-time)))
    (format t "Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))
    
(setf *t* (loadtrack "track23.txt"))
(setf *s* (initial-state *t*))
(setf *p* (make-problem :initial-state (initial-state *t*) :fn-isGoal #'isGoalp :fn-nextstates #'nextStates :fn-h #'compute-heuristic))

(let ((real1 (get-internal-real-time)))
    (format t "~&Track 23 - Heuristic~&")
    (compute-heuristic *s*)
    (format t "~&Track 23 - A*~&")
    (states-to-list (a* *p*))
    (let ((real2 (get-internal-real-time)))
    (format t "Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))
    
(setf *t* (loadtrack "track24.txt"))
(setf *s* (initial-state *t*))
(setf *p* (make-problem :initial-state (initial-state *t*) :fn-isGoal #'isGoalp :fn-nextstates #'nextStates :fn-h #'compute-heuristic))

(let ((real1 (get-internal-real-time)))
    (format t "~&Track 24 - Heuristic~&")
    (compute-heuristic *s*)
    (format t "~&Track 24 - A*~&")
    (states-to-list (a* *p*))
    (let ((real2 (get-internal-real-time)))
    (format t "Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))

; HEURISTICS
; SPEED = BAD? (avg/2)?
; ???