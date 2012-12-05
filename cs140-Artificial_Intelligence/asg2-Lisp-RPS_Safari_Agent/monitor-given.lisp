;;;; CMPS 140 Tournament Monitor
;;;; Each agent starts with 200 points. There are 1000 rounds per tournament.
(defun simple-agent (rounds scores myscore)
  '(1 R))
(defun simple-agent2 (rounds scores myscore)
  '(1 P))
(defun simple-agent3 (rounds scores myscore)
  '(1 S))
(defun illegal-agent (rounds scores myscore)
  '(0 P))

(defun verbose-agent (r s m)
  (let ((shoot (nth (random 3)
		'(R P S))) (amount (+ 1 (random 4))))
  (list amount shoot)))

(defun replace-nth (list n elem)
  (cond 
    ((null list) ())
    ((equal n 0) (cons elem (cdr list)))
    (t (cons (car list) (replace-nth (cdr list) (- n 1) elem)))))

;;; Monitor takes a list of agents, returns an ordered list of winners.
(defun sort-agents (agent-scores)
  (sort agent-scores #'(lambda (x y) (< (second x) (second y)))))

(defun compress-agents (agents)
  (setq agents (sort-agents agents))
  (let ((results (list (first (first agents)))) (lastscore (second (first agents))))
    (dolist (agnt (cdr agents))
 	(if (listp (car results))
	    (if (= (second agnt) lastscore)
		(setq results (cons (cons (first agnt) (first results)) (rest results)))
		(progn
		  (setq lastscore (second agnt))
		  (push (first agnt) results)))
	    (if (= (second agnt) lastscore)
		(setq results (cons (cons (first agnt) (cons (first results) nil)) (rest results)))
		(progn
		  (setq lastscore (second agnt))
		  (push (first agnt) results)))))
    results))

(defun legalp (bet choice score)
  (and
   (not (= bet 0))
   (or (<= (abs bet)
	  score)
       (and (<= score 0)
	    (= bet 1)))
   (member choice '(r p s))
   (not
    (or (null bet)
	(null choice)
	(null score)))))

(defun sign (input)
  (if (= input 0)
      0
      (if (< input 0)
	  -1
	  1)))

(defun tournament (agents numtimes)
  (let ((scores (make-list (length agents) :initial-element 1))
	(net-scores nil) (results nil) (legal-status (make-list (length agents) :initial-element t)))
    (print "Running tournament")
      (do ((iter 0 (setq iter (+ iter 1))))
	  ((= iter numtimes))
	(let ((num 0) (r 0) (p 0) (s 0) (current-play nil))
	    ;; Run each agent
	    (dolist (curagent agents)
	      (let* ((plyval (funcall curagent results scores (nth num scores))) (bid (first plyval)))
		(if (legalp (first plyval) (second plyval) (nth num scores))
		    (progn
		      (case (second plyval)
			(r (setq r (+ r bid)))
			(p (setq p (+ p bid)))
			(s (setq s (+ s bid)))
			(t 0))
		      (setf num (+ 1 num))
		      (push plyval current-play))
		    (progn
		      (setq legal-status (replace-nth legal-status num nil))
		      (push plyval current-play)
		      (setq num (+ 1 num))))))
	    (setq current-play (reverse current-play))
	    (dotimes (frmagent (length agents))
	      (let* ((curagent frmagent) (play (nth curagent current-play)) (shoot (second play)) (bid (first play)))
		(if (nth curagent legal-status)
		    (setq scores (replace-nth scores curagent (+ (nth curagent scores)
								 (* bid
								    (sign (case shoot
								      (r (- s p))
								      (p (- r s))
								      (s (- p r))
								      (t 0)))))))
		    (setq scores (replace-nth scores curagent 0)))))
	    (push (list r p s) results)))
      (dotimes (x (length agents))
	(if (nth x legal-status)
	    (push (list (nth x agents) (nth x scores)) net-scores)
	    nil))
      (reverse (compress-agents net-scores))
      ))
;(tournament '(simple-agent simple-agent2 simple-agent3 random-agent illegal-agent) 100)

	      
;;;; Takes a list of agents, inserts them in a hash-table and passes theo
;;;; hash-table to the tournament numtimes times
(defun monitor (agent-list numtimes)
  (let (results (agents (make-hash-table :size 100)))
  (dolist (agnt agent-list)
    (setf (gethash agnt agents) 0))
  (dotimes (x numtimes)
    (let ((curRank 0) (results (tournament agent-list 1000)))
      (dolist (agent-result results)
	  (if (listp agent-result)
	      (progn
		(dolist (curagent agent-result)
		  (setf (gethash curagent agents) (+ (gethash curagent agents) curRank)))
		(setq curRank (+ curRank (length agent-result))))
	      (progn
		(setf (gethash agent-result agents) (+ (gethash agent-result agents) curRank))
		(setq curRank (+ curRank 1)))))))
  (maphash #'(lambda (k v) (push (cons k (- (length agent-list) (ceiling (/ v numtimes)))) results)) agents)
  results))

;; Returns a list of dotted pairs, with the agent and the score (numagents - avg rank)
;(monitor '(simple-agent simple-agent2 simple-agent3 random-agent a-simple-agent a-simple-agent2 a-simple-agent3 a-random-agent) 1000)

;(monitor agentlist 1000)
