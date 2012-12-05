;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: monitor.lisp,v 1.2 2012-02-08 02:14:29-08 dmfrank - $
;; Derek Frank, dmfrank@ucsc.edu
;;
;; CMPS 140 Tournament Monitor
;; Each agent starts with 200 points. There are 1000 rounds per tournament.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;; simple-agent
;;
;; An agent that consistently bets 1 on rock each round.
;;
(defun simple-agent (rounds scores myscore)
  '(1 R))

;;;;
;; simple-agent2
;;
;; An agent that consistently bets 1 on paper each round.
;;
(defun simple-agent2 (rounds scores myscore)
  '(1 P))

;;;;
;; simple-agent3
;;
;; An agent that consistently bets 1 on scissors each round.
;;

(defun simple-agent3 (rounds scores myscore)
  '(1 S))

;;;;
;; illegal-agent
;;
;; An agent that illegally bets 0 on paper for each round.
;; Used to test the legalp function.
;;
(defun illegal-agent (rounds scores myscore)
  '(0 P))

;;;;
;; random-agent
;;
;; An agent that bets one each round, but uses a pseudo-random number
;; generator to pick rock, paper, or scissors.
;;
(defun random-agent (rounds scores myscore)
  (list '1 (nth (random 3)
		'(R P S))))

;;;;
;; verbose-agent
;;
;; An agent that uses the pseudo-random number generator to both choose
;; to play rock, paper, or scissor and an amount to bet ranging from
;; one to four.
;;
(defun verbose-agent (r s m)
  (let ((shoot (nth (random 3) '(R P S))) (amount (+ 1 (random 4))))
    (list amount shoot)))

;;;;
;; replace-nth
;;
;; Replaces an element of a given list at the given index with a given
;; element.  Returns this new list.
;;
(defun replace-nth (list n elem)
  (cond 
    ((null list) ())
    ((equal n 0) (cons elem (cdr list)))
    (t (cons (car list) (replace-nth (cdr list) (- n 1) elem)))))

;;;;
;; sort-agents
;;
;; Monitor takes a list of agents, returns an ordered list of winners.
;;
(defun sort-agents (agent-scores)
  (sort agent-scores #'(lambda (x y) (< (second x) (second y)))))

;;;;
;; compress-agents
;;
;; Function that takes a list of agents.  Returns 
;;
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

;;;;
;; legalp
;;
;; Function that takes a given bet amount, rock/scissor/paper choice,
;; and optional current score (optional since the first play has an
;; initial value).  It then determines if these inputs by an agent
;; qualify as a legal play.  Returns true (t) if the play is legal
;; and false (nil) if the play is illegal.
;;
(defun legalp (bet choice &optional (score 200))
  (and
    (integerp bet)
    (integerp score)
    (/= bet 0)
    (or (<= (abs bet) score)
      (and (<= score 0)
           (= (abs bet) 1)))
    (member choice '(r p s))
    (not
      (or (null bet)
          (null choice)
          (null score)))))

;;;;
;; tournament
;;
;; A tournament takes as input a list of agents and the number of
;; rounds to be played.  
;;
(defun tournament (agents numtimes)
  (let ((scores (make-list (length agents) :initial-element 200))
	(net-scores nil) (results nil) (legal-status (make-list (length agents) :initial-element t)))
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
								    (case shoot
								      (r (- s p))
								      (p (- r s))
								      (s (- p r))
								      (t 0))))))
		    (setq scores (replace-nth scores curagent 0)))))
	    (push (list r p s) results)))
      (dotimes (x (length agents))
	(if (nth x legal-status)
	    (push (list (nth x agents) (nth x scores)) net-scores)
	    nil))
      (reverse (compress-agents net-scores))))

;(tournament '(simple-agent simple-agent2 simple-agent3 random-agent illegal-agent) 100)

;;;;
;; monitor
;;
;; Takes a list of agents, inserts them in a hash-table and passes the
;; hash-table to the tournament numtimes times
;;
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
