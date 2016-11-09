;;; IA 2016/2017 tg046
;;; 78013 Bruno Henriques
;;; 82094 Leonardo Vieira

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
(defun limdepthfirstsearch (problem lim)
  "limited depth first search"
  (defun ldfsAux (node i)
    (let ((state (node-state node)) (cut? nil))
    
      (cond
        ((funcall (problem-fn-isGoal problem) state) (return-from ldfsAux node))
        ((zerop i) (return-from ldfsAux :corte)))

      (loop for st in (funcall (problem-fn-nextStates problem) state) do
        (let* ((child (make-node :state st :parent node)) (res (ldfsAux child (- i 1))))
          (if (eq res :corte)
            (setf cut? :corte)
            (unless (null res) (return-from ldfsAux res)))))
      cut?))
  
  (let ((res (ldfsAux (make-node :state (problem-initial-state problem)) lim)) (resPath))
    (when (eq (type-of res) 'NODE) ; fill path until start-node (root)
      (push (node-state res) resPath)
      (loop until (null (node-parent res)) do
        (setf res (node-parent res))
        (push (node-state res) resPath))
      (setf res resPath))
    res))


;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search"
  (loop for i from 0 to most-positive-fixnum do
    (let ((res (limdepthfirstsearch problem i)))
      (unless (eq res :CORTE) (return-from iterlimdepthfirstsearch res)))))
