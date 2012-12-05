;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: agent.lisp,v 1.3 2012-02-10 19:06:24-08 dmfrank - $
;; Derek Frank, dmfrank@ucsc.edu
;; CMPS 140 Winter 2012
;; Program 2
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun agent (rounds scores myscore)
   (let (bet choice numrounds sign avgR avgP avgS decision)
      (setf numrounds (length rounds))
      (if (< (random 1000.0) 600) (setf sign -1)
         (setf sign 1))
      (if (not (null *avg-returns*))
         (progn
            (setf avgR (car *avg-returns*))
            (setf avgP (cadr *avg-returns*))
            (setf avgS (caddr *avg-returns*))
            (setf decision (random 2))
            (cond
               ((and (= avgR avgP) (= avgR avgS) (= avgS avgP))
                  (setf choice (nth (random 3) '(R P S))) (setf bet sign))
               ((and (= avgR avgP) (> avgR avgS))
                  (setf choice 'P) (setf bet (nth (random 2) '(1 -1))))
               ((and (= avgR avgP) (< avgR avgS))
                  (cond
                     ((= decision 0) (setf choice 'S) (setf bet 1))
                     (t (setf choice 'R) (setf bet -1))))
               ((and (= avgR avgS) (> avgR avgP))
                  (setf choice 'R) (setf bet (nth (random 2) '(1 -1))))
               ((and (= avgR avgS) (< avgR avgP))
                  (cond
                     ((= decision 0) (setf choice 'P) (setf bet 1))
                     (t (setf choice 'S) (setf bet -1))))
               ((and (= avgS avgP) (> avgS avgR))
                  (setf choice 'S) (setf bet (nth (random 2) '(1 -1))))
               ((and (= avgS avgP) (< avgS avgR))
                  (cond
                     ((= decision 0) (setf choice 'R) (setf bet 1))
                     (t (setf choice 'P) (setf bet -1))))
               ((and (> avgR avgP) (> avgR avgS))
                  (cond
                     ((= decision 0) (setf choice 'R) (setf bet 1))
                     (t (setf choice 'P) (setf bet -1))))
               ((and (> avgP avgS) (> avgP avgR))
                  (cond
                     ((= decision 0) (setf choice 'P) (setf bet 1))
                     (t (setf choice 'S) (setf bet -1))))
               ((and (> avgS avgP) (> avgS avgR))
                  (cond
                     ((= decision 0) (setf choice 'S) (setf bet 1))
                     (t (setf choice 'R) (setf bet -1))))
               (t (setf choice (nth (random 3) '(R P S))) (setf bet sign))))
         (progn (setf choice (nth (random 3) '(R P S))) (setf bet sign)))
      (cond
         ((> myscore 1000) (setf bet (* bet 1)))
         ((> myscore 600) (setf bet (* bet 50)))
         ((> myscore 500) (setf bet (* bet 10)))
         ((> myscore 200) (setf bet (* bet 50)))
         ((> myscore 50) (setf bet (* bet 5)))
         ((> myscore 20) (setf bet (* bet 10)))
         ((> myscore 5) (setf bet (* bet 2)))
         (t (setf bet (* bet 1))))
      (list bet choice)))
