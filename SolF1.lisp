
;;; These functions, and any other ones needed must be implemented

(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track) 
  (let ((l (nth 0 pos)) (c (nth 1 pos)) (env (track-env track)))
    (not (nth c (nth l env)))))

(defun isGoalp (st)
  (let ((pos (state-pos st)) (ends (track-endpositions (state-track st))))
    (loop for p in ends do
      (when (equal pos p) (return-from isGoalp t)))))

(defun nextState (st act)
  (let ((pos (state-pos st)) (vel (state-vel st)) (cost 1))
    (setq newVel (list (+ (nth 0 vel) (nth 0 act)) (+ (nth 1 vel) (nth 1 act))))
    (setq newPos (list (+ (nth 0 pos) (nth 0 newVel)) (+ (nth 1 pos) (nth 1 newVel))))
    (when (isObstaclep newPos (state-track st))
      (setq newPos pos) (setq newVel '(0 0)) (setq cost 20))
    (when (isGoalp (make-state :POS newPos :TRACK (state-track st))) (setq cost -100)) ; ??
    (make-STATE :POS newPos :VEL newVel :ACTION act :COST cost :TRACK (state-track st))))
