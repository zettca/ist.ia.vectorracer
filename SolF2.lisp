(load "datastructures.lisp")
(load "auxfuncs.lisp")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st)
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir
(defun nextStates (st)
  "generate all possible next states"
  (when (isObstaclep (state-pos st) (state-track st))
    (return-from nextStates nil)) ; idk about this
  (let ((actions (possible-actions)) (res '()))
    (dolist (act actions)
      (push (nextState st act) res))
  res))

;;; limdepthfirstsearch
(defun limdepthfirstsearch (problem lim &key cutoff?)
  "limited depth first search"
  (let ((fn-nextStates (problem-fn-nextStates problem))
        (fn-isGoal (problem-fn-isGoal problem)))
  
  (defun limdepthfirstsearchAux (node prob n)
    (let* ((st (node-state node)) (isCutoff nil))

    (cond
      ((funcall fn-isGoal st) (return-from limdepthfirstsearchAux (list (node-state node))))
      ((zerop n) (return-from limdepthfirstsearchAux :cutoff))) ; cutoff ?

    ;(format t "DEPTH: ~a | POSITION: ~a~%" n (state-pos (node-state node)))
    (loop for state in (funcall fn-nextStates st) do
      (progn
        (let* ((child (make-node :state state :parent node))
              (res (limdepthfirstsearchAux child prob (- n 1))))
          (if (equal res :cutoff)
            (setf isCutoff t)
            (unless (null res)
              (return-from limdepthfirstsearchAux (cons (node-state child) res)))))))
    (if isCutoff :cutoff nil)))

  ;(list (make-node :state (problem-initial-state problem))))
  
	(limdepthfirstsearchAux (make-node :state (problem-initial-state problem)) problem lim)))


;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search"
	;(list (make-node :state (problem-initial-state problem)))
	nil)
