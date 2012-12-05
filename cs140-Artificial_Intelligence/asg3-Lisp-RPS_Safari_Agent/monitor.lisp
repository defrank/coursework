;;;; CMPS 140 Tournament Monitor
;;;; Version 1.3 (3/2/2012)
;;;; Each agent starts with 1 point. There are 1000 rounds per tournament.
(in-package :class-monitor)
(defparameter *avg-returns* '(0 0 0))
(defparameter *total-returns* '(0 0 0))
(defparameter *illegal-agents* nil)
(defparameter *grad-list* (list 'khuang7:agent 'mleece:agent 'tkaraden:agent 'tmarlett:agent 'tsarratt:agent 'mhultqui:agent))
(defparameter *current-ruleset* :p2)

;; Adds all agents of the same score to a list for ranking purposes in a final list of results. Preserves order
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

;; Current test for legality. Assumes agents behave well in terms of returning two element lists.
(defun legalp (bet choice score)
  (and
   (integerp bet)
   (not (= bet 0))
   (or (<= (abs bet)
	   score)
       (and (<= score 0)
	    (= (abs bet) 1)))
   (member choice '(r p s))
   (not
    (or (null bet)
	(null choice)
	(null score)))))

;; Changes the list in memory destructively.
(defun replace-nth-dest (list n elem)
  (setf (nth n list) elem))

(defun replace-nth (list n elem)
  (if (< n (length list))
      (cond 
	((null list) ())
	((equal n 0) (cons elem (cdr list)))
	(t (cons (car list) (replace-nth (cdr list) (- n 1) elem))))
      (progn
	(format t "incorrect array access")
      )))

;;; Monitor takes a list of agents, returns an ordered list of winners.
(defun sort-agents (agent-scores)
  (sort (copy-list agent-scores) #'(lambda (x y) (< (second x) (second y)))))


;; Returns an integer value, instead of the float provided by signum
(defun sign (input)
  (if (= input 0)
      0
      (if (< input 0)
	  -1
	  1)))


;; From https://groups.google.com/group/comp.lang.lisp/browse_thread/thread/a925ca60d88d4047/cc06e579dfe840a7?lnk=gst&q=displaced+array
(defun coerce-array-to-list (in-array) 
   (map 'list 
           #'identity 
           (make-array (array-total-size in-array) 
                             :element-type (array-element-type in-array) 
                             :displaced-to in-array))) 

;; Used to abstract away each version of the ruleset.
(defun calcscore (bid shoot rs ps ss)
  (case *current-ruleset*
    (:p2
      (* bid
	 (sign (case shoot
		 (r (- ss ps))
		 (p (- rs ss))
		 (s (- ps rs))
		 (otherwise 0))))
      )
    (:p3
     (- (* bid
	   (signum (case shoot
		     (r (- ss ps))
		     (p (- rs ss))
		     (s (- ps rs))
		     (otherwise 0))))
	(* (signum bid)
	   (case shoot
	     (r (first *total-returns*))
	     (p (second *total-returns*))
	     (s (third *total-returns*))
	     (otherwise 0)))))
    (otherwise 0)))

;; Executes a single tournament with numtimes rounds.
(defun tournament (agents numtimes)
  (let ((scores (make-array (length agents) :initial-element 1))
	(net-scores nil) (results nil)
	(legal-status (make-array (length agents) :initial-element t)))
    (setf *avg-returns* (make-list (+ (length agents) 3) :initial-element 0))
    (setf *total-returns* (make-list (+ (length agents) 3) :initial-element 0))
					;    (print "Running tournament")
    (do ((iter 0 (setq iter (+ iter 1))))
	((= iter numtimes))
      (let ((num 0) (r 0) (p 0) (s 0) (current-play nil) (scoreslist (coerce-array-to-list scores)))
	;; Run each agent
	(dolist (curagent agents)
	  (let* ((plyval (funcall curagent results scoreslist (aref scores num))) (bid (first plyval)))
	    (if (aref legal-status num)
		(progn
		  (if (legalp (first plyval) (second plyval) (aref scores num))
		      (case (second plyval)
			(r (setq r (+ r bid)))
			(p (setq p (+ p bid)))
			(s (setq s (+ s bid)))
			(t 0))
		      (progn
			(setf (aref legal-status num) nil)
			(format t "~S played illegally: ~S for ~S. Had ~S
" curagent (second plyval) bid (aref scores num))
			(push curagent *illegal-agents*)))))
	    (setf num (+ 1 num))
	    (push plyval current-play) ))
	(replace-nth-dest *total-returns* 0  (+ (first *total-returns*) (sign (- s p))))
	(replace-nth-dest *total-returns* 1 (+ (second *total-returns*) (sign (- r s))))
	(replace-nth-dest *total-returns* 2 (+ (third *total-returns*) (sign (- p r))))
	(replace-nth-dest *avg-returns* 0 (/ (first *total-returns*) (+ iter 1.0)))
	(replace-nth-dest *avg-returns* 1 (/ (second *total-returns*) (+ iter 1.0)))
	(replace-nth-dest *avg-returns* 2 (/ (third *total-returns*) (+ iter 1.0)))
	(setq current-play (reverse current-play))
;	(format t "Round over:
;~S
;~S
;R: ~S, P: ~S, S:~S
;	" *total-returns* *avg-returns* r p s )
	(dotimes (frmagent (length agents))
	  (let* ((curagent frmagent) (play (nth curagent current-play)) (shoot (second play)) (bid (if (> (abs (first play)) 1000000) (* (sign (first play)) 100000) (first play)))
		 (curchange (calcscore bid shoot r p s)))
	    (setf (aref scores curagent) (+ (aref scores curagent) curchange))
	    (replace-nth-dest *total-returns* (+ curagent 3) (+ (nth (+ curagent 3) *total-returns*) curchange))
	    (replace-nth-dest *avg-returns* (+ curagent 3) (/ (nth (+ curagent 3) *total-returns*) (+ iter 1.0)))))
	(push (list r p s) results)))
	(dotimes (x (length agents))
	  (if (aref legal-status x)
	      (push (list (nth x agents) (aref scores x)) net-scores)
	      (format t "~S is ILLEGAL!" (nth x agents)	      )
	      ))
	(format t "
================================================================================
=                         Tournament Scores:                                   =
================================================================================
")
	(mapcar (createformatter 1) (sort (copy-list net-scores) #'(lambda (x y) (> (cadr x) (cadr y)))))
	(compress-agents net-scores)))
;; Function for pretty printing tourny results. Displays numcol lists per line, with
;; second element of list formatted to be one decimal place.
(defun createformatter (numcol)
  (let ((row 1))
    (lambda (k) (format t "(~S ~1$)" (first k) (second k))
	    (if (= (mod row numcol) 0)
		(format t "
"))
	    (setq row (+ row 1)))))

;; Predicate to determine whether a given agent is in the grad list.
(defun gradp (x) (member x *grad-list*))


;;;; Takes a list of agents, inserts them in a hash-table which stores
;;;; the total results for each tournament as well as for the entire
;;;; set.
(defun monitor (agent-list numtimes)
  (let (total-results total-results-grads (agents (make-hash-table :size 100)))
    (setf *illegal-agents* nil)
    (dolist (agnt agent-list)
      (setf (gethash agnt agents) 0))
    (dotimes (x numtimes)
      (let ((curRank 0.0) (curRankGrad 0.0) (curresults (tournament agent-list 1000)) tournyresults tournyresultsgrads)
	(dolist (agent-result curresults)
	  (if (listp agent-result)
	      (progn 
		(dolist (curagent agent-result)
		  (let ((isgrad (gradp curagent))
			(ugradRank (+ curRank (/ (- (length (remove-if #'gradp agent-result)) 1) 2)))
			(gradRank (+ curRankGrad (/ (- (length agent-result) 1) 2))))
		    (if (not (member curagent *illegal-agents*))
			(progn
			  ;; Give each agent in that list the average rank
			  (if isgrad
			      (progn (push (list curagent gradRank) tournyresultsgrads)
					 ;; Add to hash grad
				     (setf (gethash curagent agents)
					   (+ (gethash curagent agents)
					      gradRank))
				     )
			      ;; Add to hash undergrad
			      (progn
				(setf (gethash curagent agents)
				      (+ (gethash curagent agents)
					 ugradRank))
				(push (list curagent ugradRank) tournyresults)
				))))))
		(setq curRankGrad (+ curRankGrad (length agent-result)))
		(setq curRank (+ curRank (length (remove-if #'gradp agent-result)))))
	      (if (not (member agent-result *illegal-agents*))
		  (if (gradp agent-result)
		      (progn
			(push (list agent-result curRankGrad) tournyresultsgrads)
			(setf (gethash agent-result agents)
			      (+ (gethash agent-result agents)
				 curRankGrad))
			(setq curRankGrad (+ curRankGrad 1.0)))
		      (progn
			(push (list agent-result curRank) tournyresults )
			(setf (gethash agent-result agents)
			      (+ (gethash agent-result agents)
				 curRank))
			;; Increase ranks for both undergrads and grads
			(setq curRankGrad (+ curRankGrad 1.0))
			(setq curRank (+ curRank 1.0)))
		      ))))
	;; Remove all of the illegal agents from hash table and results
	(format t "Removing illegal agents: ~S" *illegal-agents*)
	(dolist (ill-agent *illegal-agents*)
	  (setf agent-list (remove ill-agent agent-list))
	  (remhash ill-agent agents)
	  )
	(setf *illegal-agents* nil)
	(format t "
================================================================================
===                         TOURNAMENT # ~S                                  ===
================================================================================

Total returns: ~S
Average returns: ~S

================================================================================
===               Results (Ranks, lower is better)                           ===
================================================================================

Graduates:

" x *total-returns* *avg-returns*)
	(mapcar (createformatter 3) (reverse tournyresultsgrads))
	(format t "

Undergraduates:

")
	(mapcar (createformatter 3) (reverse tournyresults))))
    (maphash #'(lambda (k v)
		 (if (not (gradp k))
		     (push (cons k
				 (- (length (remove-if #'gradp agent-list))
				    (/ v (float numtimes))))
			   total-results)))
	     agents)
    (setq total-results (sort (copy-list total-results)  #'(lambda (x y) (> (cdr x) (cdr y)))))
    (maphash #'(lambda (k v)
		 (push (cons k (- (length agent-list)
				  (/ v (float numtimes))))
		       total-results-grads))
	     agents)
    (setq total-results-grads (sort (copy-list total-results-grads) #'(lambda (x y) (> (cdr x) (cdr y)))))
    (let ((endrank (length total-results))
	  (endrankgrads (length total-results-grads))
	  (curscore -10000000000)
	  (curscoregrad -10000000000)
	  finalresults
	  finalresultsgrads)
      (setq curscore (cdr (car total-results)))
      (setq curscoregrad (cdr (car total-results-grads)))
      (print "begin list")
      (dolist (agent-result total-results)
	(if (not (eql curscore (cdr agent-result)))
	    (setq endrank (- endrank 1)))

	(push (list (car agent-result) (/ endrank (/ (length total-results) 35))) finalresults)
	(format t "curscore: ~S
" curscore)
	(format t "agent-result: ~S
" agent-result)
	(format t "endrank: ~S
" endrank)
	(setq curscore (cdr agent-result)))
      (dolist (agent-result total-results-grads)
	(if (not (eql curscoregrad (cdr agent-result)))
	    (setq endrankgrads (- endrank 1)))
	(push (list (car agent-result) (/ endrankgrads (/ (length total-results-grads) 35))) finalresultsgrads)
	(format t "curscore: ~S
" curscoregrad)
	(format t "agent-result: ~S
" agent-result)
	(format t "endrank: ~S
" endrankgrads)
	(setq curscoregrad (cdr agent-result)))
      (setq finalresultsgrads (sort (copy-list finalresultsgrads) #'(lambda (x y) (> (second x) (second y)))))
      (setq finalresults (sort (copy-list finalresults)  #'(lambda (x y) (> (second x) (second y)))))
      (format t "

********************************************************************************
**              Scores: ((# of agents) - rank) / (# of agents / 35)            *
********************************************************************************

================================================================================
=                   Final Monitor Results (higher is better)                   =
================================================================================
Grads:
")
      (mapcar (createformatter 3) (remove-if-not #'(lambda (a) (gradp (first a))) finalresultsgrads))
      (format t "

Undergraduates:

")
      (mapcar (createformatter 3) finalresults)
      nil)))
