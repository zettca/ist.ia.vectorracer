;;; IA 2016/2017 tg046
;;; 78013 Bruno Henriques
;;; 82094 Leonardo Vieira

(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track)
    (not (nth (second pos) (nth (first pos) (track-env track)))))

(defun isGoalp (st)
  (loop for p in (track-endpositions (state-track st)) do
    (when (equal (state-pos st) p) (return-from isGoalp t))))

(defun nextState (st act)
  (let* ((pos (state-pos st)) (vel (state-vel st)) (cost 1)
    (newVel (list (+ (nth 0 vel) (nth 0 act)) (+ (nth 1 vel) (nth 1 act))))
    (newPos (list (+ (nth 0 pos) (nth 0 newVel)) (+ (nth 1 pos) (nth 1 newVel)))))
    (when (isObstaclep newPos (state-track st)) (setq newPos pos newVel '(0 0) cost 20))
    (when (isGoalp (make-state :POS newPos :TRACK (state-track st))) (setq cost -100))
    (make-STATE :POS newPos :VEL newVel :ACTION act :COST cost :TRACK (state-track st))))
