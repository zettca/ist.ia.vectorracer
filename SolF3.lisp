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

(defun nodeToStateList (node)
  (let ((res (list node)))
    (loop until (null (node-parent (first res))) do
      (push (node-parent (first res)) res))
    (mapcar #'(lambda (node) (node-state node)) res)))

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
      (setf res (nodeToStateList res)))
    res))

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search"
  (loop for i from 0 to most-positive-fixnum do
    (let ((res (limdepthfirstsearch problem i)))
      (unless (eq res :CORTE) (return-from iterlimdepthfirstsearch res)))))


;; Solution of phase 3
(defun notintegerp (el)
  (not (integerp el)))

(defun get2d (lst2d pos)
  (nth (pos-c pos) (nth (pos-l pos) lst2d)))

(defun set2d (lst2d pos val)
  (setf (nth (pos-c pos) (nth (pos-l pos) lst2d)) val))

(defun validPosp (pos)
  (and (>= (pos-l pos) 0) (>= (pos-c pos) 0)))

(defun worseAdjPosp (value dist)
  (or (eq value t) (and value (< dist value))))

(defun adjPoss (pos)
  "returns all valid adjacent positions given a position"
  (loop for adj in '((1 -1) (-1 1) (1 1) (-1 -1) (0 1) (1 0) (-1 0) (0 -1))
    when (validPosp (mapcar #'+ pos adj)) collect (mapcar #'+ pos adj)))

(defun update-bfs (map adjs dist)
  "BFS implementation for computing heuristic"
  (defun validAdjs (poss)
    (defun setPosRetAdjs (pos) (set2d map pos dist) (adjPoss pos))
    (loop for pos in poss when (worseAdjPosp (get2d map pos) dist)
      append (setPosRetAdjs pos)))
  (let ((newAdjs (remove-duplicates (validAdjs adjs) :test #'equal)))
    (when (> (length newAdjs) 0) (update-bfs map newAdjs (+ dist 1)))))

(defun update-dfs (map pos dist)
  "DFS implementation for computing heuristic"
  (set2d map pos dist)
  (loop for adj in (adjPoss pos) do
    (when (worseAdjPosp (get2d map adj) (+ dist 1))
      (update-dfs map adj (+ dist 1)))))

;; Heuristic
(defun compute-heuristic (st)
  "returns the shortest distance to the closest endposition for a given state"
  (let ((map (copy-list (track-env (state-track st)))))
    (loop for end in (track-endpositions (state-track st)) do
      (set2d map end 0) (update-bfs map (adjPoss end) 1)) ; BFS IMPLEMENTATION
      ;(update-dfs map end 0))  ; DFS IMPLEMENTATION
    (dotimes (i (first (track-size (state-track st))))
      (setf (nth i map) (substitute-if most-positive-fixnum #'notintegerp (nth i map))))
    (get2d map (state-pos st))))

;;; A*
(defun a* (problem)
  "solves a problem using A* search returning a list of states"
  (let ((frontier nil)
    (state (problem-initial-state problem))
    (nextStates (problem-fn-nextStates problem))
    (isGoal (problem-fn-isGoal problem))
    (h (problem-fn-h problem)))

    (defun aAux (node)
      (when (funcall isGoal (node-state node)) (return-from aAux node))
      (loop for st in (funcall nextStates (node-state node)) do
        (push (make-node :parent node :state st :g (+ (node-g node) (state-cost st))
          :f (+ (node-g node) (state-cost st) (funcall h st))) frontier))

      (when (null frontier) (return-from aAux nil))
      (let ((best (first frontier)))
        (loop for open in frontier do
          (when (< (node-f open)(node-f best)) (setf best open)))
        
        (setf frontier (remove best frontier))
        (aAux best)))

    (nodeToStateList (aAux (make-node :parent nil :state state :g 0 :f (funcall h state))))))

(defun best-search (problem)
  "solves a problem using an alternative search returning a list of states"
  (let ((frontier nil)
    (state (problem-initial-state problem))
    (nextStates (problem-fn-nextStates problem))
    (isGoal (problem-fn-isGoal problem))
    (h (problem-fn-h problem)))

    (defun bestAux (node)
      (when (funcall isGoal (node-state node)) (return-from bestAux node))
      (loop for st in (funcall nextStates (node-state node))
        when (not (equal (state-pos st) (state-pos (node-state node)))) do
          (push (make-node :parent node :state st :g (+ (node-g node) (state-cost st))
            :f (+ (node-g node) (state-cost st) (funcall h st))) frontier))

      (when (null frontier) (return-from bestAux nil))
      (let ((best (first frontier)))
        (loop for open in frontier do
          (when (< (node-f open) (node-f best)) (setf best open)))
        
        (setf frontier (remove best frontier))
        (bestAux best)))

    (nodeToStateList (bestAux (make-node :parent nil :state state :g 0 :f (funcall h state))))))
