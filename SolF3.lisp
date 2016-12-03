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

(defun nodeToList (node)
  (let ((res (list node)))
    (loop until (null (node-parent (first res))) do
      (push (node-parent (first res)) res))
    (setf res (mapcar #'(lambda (el) (node-state el)) res))
    res))

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
  (reverse (loop for act in (possible-actions)
    collect (nextState st act))))

;;; limdepthfirstsearch
(defun limdepthfirstsearch (problem lim)
  "limited depth first search"
  (defun ldfsAux (node i)
    (let ((state (node-state node)) (fail? nil))

      (cond
        ((funcall (problem-fn-isGoal problem) state) (return-from ldfsAux node))
        ((zerop i) (return-from ldfsAux :corte)))

      (loop for st in (funcall (problem-fn-nextStates problem) state) do
        (let* ((child (make-node :state st :parent node)) (res (ldfsAux child (- i 1))))
          (if (eq res :corte)
            (setf fail? :corte)
            (unless (null res) (return-from ldfsAux res)))))
      fail?))

  (let ((res (ldfsAux (make-node :state (problem-initial-state problem)) lim)))
    (when (eq (type-of res) 'NODE) ; fill path until start-node (root)
      (setf res (nodeToList res)))
    res))

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search"
  (loop for i from 0 to most-positive-fixnum do
    (let ((res (limdepthfirstsearch problem i)))
      (unless (eq res :CORTE) (return-from iterlimdepthfirstsearch res)))))


;; Solution of phase 3

;; Heuristic
(defun compute-heuristic (st)
  (let* ((trk (state-track st)) (sPos (state-pos st))
        (map (copy-list (track-env trk))) (ends (track-endpositions trk)))

    (defun adjposs (pos)
      (loop for adj in '((0 1) (1 0) (-1 0) (0 -1) (1 -1) (-1 1) (1 1) (-1 -1)) collect
        (mapcar #'+ pos adj)))

    (defun updateAjds (pos dist)
      (setf (nth (second pos) (nth (first pos) map)) dist)
      ;(format t "POS: ~a DIST: ~a~%" pos dist)
      (let ((distt (+ dist 1)))
        (loop for adjPos in (adjposs pos) do
          (let ((adjVal (nth (second adjPos) (nth (first adjPos) map))))
            (when (or (eq adjVal t) (and adjVal (< distt adjVal)))
              (updateAjds adjPos distt))))))

    (loop for end in ends do
      (updateAjds end 0))

    (dotimes (i (first (track-size trk)))
      (setf (nth i map) (substitute most-positive-fixnum nil (nth i map))))

    (nth (second sPos) (nth (first sPos) map))))

;;; A*
(defun a* (problem)
  (let* ((openList nil)
    (state (problem-initial-state problem))
    (nextStates (problem-fn-nextStates problem))
    (isGoal (problem-fn-isGoal problem))
    (h (problem-fn-h problem)))

    (defun aAux (node)
      (when (funcall isGoal (node-state node)) (return-from aAux node))
      (loop for st in (funcall nextStates (node-state node)) do
        (unless (equal (state-pos st) (state-pos (node-state node)))
          (push (make-node :parent node :state st :h (funcall h st)
            :g (+ (state-cost (node-state node)) (state-cost st))
            :f (+ (state-cost (node-state node)) (state-cost st) (funcall h st)))
            openList)))

      (let ((best (first openList)))
        (loop for open in openList do
          (when (< (node-f open) (node-f best)) (setf best open)))
        
        ;(format t "POS: ~a VEL: ~a G: ~a F: ~a ~%~%" (state-pos (node-state best)) (state-vel (node-state best)) (node-g best) (node-f best))
        (when (null best) (return-from aAux nil))
        (setf openList (remove best openList))
        (aAux best)))

    (nodeToList (aAux (make-node :parent nil :state state :g 0 :h (funcall h state) :f (+ 0 (funcall h state)))))))

(defun best-search (problem)
  (a* problem))
