;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: dmfrank-p4.lisp,v 1.1 2012-03-16 20:09:31-07 dmfrank - $
;; Derek Frank, dmfrank@ucsc.edu
;; CMPS 140 Winter 2012
;; Program 4
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local variables to agent function
(let ((hist-info
        (list              ;; historical-agent performance
          (list 0 '(1 R))    ;; historical-agent
          (list              ;; sub-historical
            (list 0 '(1 R))    ;; HISTORICAL 1
            (list 0 '(1 R))    ;; HISTORICAL 2
            (list 0 '(1 R))    ;; HISTORICAL 4
            (list 0 '(1 R)))   ;; HISTORICAL 8
          '()))
      (strat-info
        (list (list        ;; sub-strategic
                (list 0 '(1 R))    ;; first-guess
                (list 0 '(1 R))    ;; second-guess
                (list 0 '(1 R))    ;; third-guess
                (list 0 '(1 R)))))) ;; fourth-guess
  ;;;;
  ;; AGENT - function
  (defun dmfrank-p4 (history scores myscore)
  ;;;;;;
  ;;; HELPER FUNCTIONS
    ;;;;
    ;; FEEDBACK - performance
    (defun feedback (play totals)
      (if (or (null play) (null totals)) 0
        (let ((sign (pop play)) (pick (pop play))
              (totR (pop totals)) (totP (pop totals)) (totS (pop totals)))
          (if (>= sign 1)
            (cond
              ((eql pick 'R) (cond ((> totS totP) 1) ((> totP totS) -1/4) (t 1/2)))
              ((eql pick 'P)(cond ((> totR totS) 1) ((> totS totR) -1/4) (t 1/2)))
              (t (cond ((> totP totR) 1) ((> totR totP) -1/4) (t 1/2))))
            (cond
              ((eql pick 'R) (cond ((< totS totP) 1) ((< totP totS) -1/4) (t 1/2)))
              ((eql pick 'P) (cond ((< totR totS) 1) ((< totS totR) -1/4) (t 1/2)))
              (t (cond ((< totP totR) 1) ((< totR totP) -1/4) (t 1/2))))))))
    ;;;;
    ;; WHO-WON - strategic
    (defun who-won (totals)
      (if (not (null totals))
        (let ((totR (pop totals)) (totP (pop totals)) (totS (pop totals))
              (winners '()))
          (cond ((and (= totR totS) (< totP totR totS))
             (push '(1 R) winners)  (push '(-1 S) winners))
            ((and (= totR totS) (> totP totR totS))
             (push '(1 S) winners)  (push '(-1 R) winners))
            ((and (= totR totP) (< totS totR totP))
             (push '(1 P) winners)  (push '(-1 R) winners))
            ((and (= totR totP) (> totS totR totP))
             (push '(1 R) winners)  (push '(-1 P) winners))
            ((and (= totP totS) (< totR totP totS))
             (push '(1 S) winners)  (push '(-1 P) winners))
            ((and (= totP totS) (> totR totP totS))
             (push '(1 P) winners)  (push '(-1 S) winners))
            ((and (> totS totR totP) (> totR totP))
             (push '(-1 S) winners) (push '(-1 P) winners) (push '( 1 R) winners))
            ((and (> totR totP totS) (> totP totS))
             (push '(-1 R) winners) (push '(-1 S) winners) (push '( 1 P) winners))
            ((and (> totP totS totR) (> totS totR))
             (push '(-1 P) winners) (push '(-1 R) winners) (push '( 1 S) winners))
            ((and (> totP totR totS) (> totR totS))
             (push '( 1 P) winners) (push '( 1 S) winners) (push '(-1 R) winners))
            ((and (> totS totP totR) (> totP totR))
             (push '( 1 S) winners) (push '( 1 R) winners) (push '(-1 P) winners))
            ((and (> totR totS totP) (> totS totP))
             (push '( 1 R) winners) (push '( 1 P) winners) (push '(-1 S) winners))
            (t nil))
        winners)))
    ;;;;
    ;; BEATS-PICK - strategic
    (defun beats-pick (picks)
      (let ((beats '()) x y)
        (cond
          ((or (= (length picks) 3) (= (length picks) 1)) (setf x (pop picks))
           (cond
             ((equal x '( 1 R)) (push '(1 P) beats) (push '(-1 S) beats))
             ((equal x '( 1 P)) (push '(1 S) beats) (push '(-1 R) beats))
             ((equal x '( 1 S)) (push '(1 R) beats) (push '(-1 P) beats))
             ((equal x '(-1 R)) (push '(1 S) beats) (push '(-1 P) beats))
             ((equal x '(-1 P)) (push '(1 R) beats) (push '(-1 S) beats))
             ((equal x '(-1 S)) (push '(1 P) beats) (push '(-1 R) beats))
             (t (push (random-agent) beats))))
          ((= (length picks) 2) (setf x (pop picks)) (setf y (pop picks))
            (cond
              ((and (equal x '(-1 R)) (equal y '(1 S))) (push '(-1 P) beats))
              ((and (equal x '(-1 R)) (equal y '(1 P))) (push '( 1 S) beats))
              ((and (equal x '(-1 P)) (equal y '(1 R))) (push '(-1 S) beats))
              ((and (equal x '(-1 P)) (equal y '(1 S))) (push '( 1 R) beats))
              ((and (equal x '(-1 S)) (equal y '(1 R))) (push '( 1 P) beats))
              ((and (equal x '(-1 S)) (equal y '(1 P))) (push '(-1 R) beats))
              (t (push (random-agent) beats))))
          (t (push (random-agent) beats)))
        beats))
    ;;;;
    ;; DECIDE - "first-guess" - strategic
    (defun decide (picks)
      (if (= (length picks) 2) (setf picks (nth (random 2) picks))
        (setf picks (car picks))) picks)
    ;;;;
    ;; SECOND-GUESS - strategic
    (defun second-guess (picks)
      (let (select) (setf select (beats-pick picks))
        (setf select (decide select)) select))
    ;;;;
    ;; THIRD-GUESS - strategic
    (defun third-guess (picks)
      (let (select)
        (setf select (beats-pick picks)) (setf select (beats-pick select))
        (setf select (decide select)) select))
    ;;;;
    ;; FOURTH-GUESS -strategic
    (defun fourth-guess (picks)
      (let (select) (setf select (beats-pick picks))
        (setf select (beats-pick select)) (setf select (beats-pick select))
        (setf select (decide select)) select))
    ;;;;
    ;; GUESS - strategic
    (defun guess (play n)
      (let (pick)
        (case n
          (2 (setf pick (second-guess play)))
          (3 (setf pick (third-guess play)))
          (4 (setf pick (fourth-guess play)))
          (t (setf pick (decide play))))
        pick))
    ;;;;
    ;; EMA - Exponential Moving Average -numerical
    (defun ema (new old n)
      (let (wgt result)
        (setf wgt (/ 2 (+ n 1))) (setf result (+ (* wgt new) (* (- 1 wgt) old)))
        result))
    ;;;;
    ;; UPDATE-PERF - performance
    (defun update-perf (perf-info totals n)
      (let ((num (pop perf-info)) (play (pop perf-info)))
        (setf num (ema (feedback play totals) num n))
        (list num play)))
    ;;;;
    ;; ATTAIN-STRAT - strategic
    (defun attain-strat (sub-perf totals)
      (let ((sub1 (pop sub-perf)) (sub2 (pop sub-perf)) (sub3 (pop sub-perf))
            (sub4 (pop sub-perf)) perf1 perf2 perf3 perf4 gn)
        ;; update subroutine performances
        (if (not (null totals))
          (progn
            (setf sub1 (update-perf sub1 totals 2))
            (setf sub2 (update-perf sub2 totals 2))
            (setf sub3 (update-perf sub3 totals 2))
            (setf sub4 (update-perf sub4 totals 2))))
        (setf perf1 (car sub1)) (setf perf2 (car sub2))
        (setf perf3 (car sub3)) (setf perf4 (car sub4))
        (cond
          ((>= perf3 perf2 perf1 perf4 1/20) (setf gn 3))
          ((>= perf4 perf2 perf3 perf1 1/20) (setf gn 4))
          ((>= perf2 perf1 perf3 perf4 1/20) (setf gn 2))
          ((>= perf1 perf2 perf3 perf4 1/20) (setf gn 1))
          (t (setf gn (nth (random 4) '(4 3 2 1)))))
        (push sub4 sub-perf) (push sub3 sub-perf) (push sub2 sub-perf)
        (push sub1 sub-perf)
        (list sub-perf gn)))
    ;;;;
    ;; SET-STRAT
    (defun set-strat (sub play)
      (let ((sub1 (pop sub)) (sub2 (pop sub)) (sub3 (pop sub)) (sub4 (pop sub)))
        (setf (cadr sub4) (guess play 4)) (push sub4 sub)
        (setf (cadr sub3) (guess play 3)) (push sub3 sub)
        (setf (cadr sub2) (guess play 2)) (push sub2 sub)
        (setf (cadr sub1) (guess play 1)) (push sub1 sub)
        sub))
    ;;;;
    ;; RANDOM-PICK
    (defun random-pick (totR totP totS)
      (let (sign pick)
        (setf pick (nth (random 3) '(R P S)))
        (case pick
          (R (if (> totR 0) (setf sign -1) (setf sign 1)))
          (P (if (> totP 0) (setf sign -1) (setf sign 1)))
          (t (if (> totS 0) (setf sign -1) (setf sign 1))))
        (list sign pick)))
    ;;;;
    ;; RANDOM-AGENT
    (defun random-agent (&optional)
      (let (sign pick)
        (setf sign (nth (random 2) '(1 -1))) (setf pick (nth (random 3) '(R P S)))
        (list sign pick)))
    ;;;;
    ;; HISTORICAL-AGENT
    (defun historical-agent (sub-perf mem hist gn)
      (defun hist-helper (matches-req past mem)
        (defun compress-hits (cmp hits)
          (apply #'+ (mapcar #'(lambda (x) (if (equal x cmp) 1 0)) hits)))
        (defun make-cmp-list (list n)
          (cond ((< (length list) n) '(()))
            ((= (length list) n) (mapcar #'(lambda (x y) x) list (make-list n)))
            (t (append (mapcar #'(lambda (x y) x) list (make-list n))
                 (make-cmp-list (cdr list) n)))))
        (let (play seq cmp-seq cmp-mem hits r p s nr np ns rns pnr snp rnp pns snr)
          ;; length of seq should be equal to matches-req
          (cond
            ((and (= (length past) 1) (> (length mem) 8)) (setf matches-req 1))
            ((> matches-req (length past)) (setf matches-req (- (length past) 1)))
            (t nil))
          ;; determine best historical play
          (cond
            ((> matches-req 0)
             ;; make lists searchable for goal
             (setf seq (mapcar #'(lambda (x y) x) past (make-list matches-req)))
             (setf seq (mapcar #'(lambda (x) (who-won x)) seq))
             (setf cmp-seq (mapcar #'(lambda (x) (who-won x)) (cdr past)))
             (setf cmp-seq (make-cmp-list cmp-seq matches-req))
             (setf cmp-mem (mapcar #'(lambda (x) (who-won x)) (cdr mem)))
             (setf cmp-mem (make-cmp-list cmp-mem matches-req))
             (setf past (mapcar #'(lambda (x) (who-won x)) past))
             (setf mem (mapcar #'(lambda (x) (who-won x)) mem))
             (setf hits
               (append
                 (mapcar #'(lambda (x y) (if (equal seq y) x nil)) past cmp-seq)
                 (mapcar #'(lambda (x y) (if (equal seq y) x nil)) mem cmp-mem)))
             ;; get a count for each combination of wins 
             (setf r (compress-hits '((1 R) (-1 P) (-1 S)) hits))
             (setf p (compress-hits '((1 P) (-1 S) (-1 R)) hits))
             (setf s (compress-hits '((1 S) (-1 R) (-1 P)) hits))
             (setf nr (compress-hits '((-1 R) (1 S) (1 P)) hits))
             (setf np (compress-hits '((-1 P) (1 R) (1 S)) hits))
             (setf ns (compress-hits '((-1 S) (1 P) (1 R)) hits))
             (setf rns (compress-hits '((-1 S) (1 R)) hits))
             (setf pnr (compress-hits '((-1 R) (1 P)) hits))
             (setf snp (compress-hits '((-1 P) (1 S)) hits))
             (setf rnp (compress-hits '((-1 P) (1 R)) hits))
             (setf pns (compress-hits '((-1 S) (1 P)) hits))
             (setf snr (compress-hits '((-1 R) (1 S)) hits))
             ;; choose most occurring play
             (cond
               ((>= r p s nr np ns rns pnr snp rnp pns snr) (setf play (list '( 1 R))))
               ((>= p r s nr np ns rns pnr snp rnp pns snr) (setf play (list '( 1 P))))
               ((>= s r p nr np ns rns pnr snp rnp pns snr) (setf play (list '( 1 S))))
               ((>= nr r p s np ns rns pnr snp rnp pns snr) (setf play (list '(-1 R))))
               ((>= np r p s nr ns rns pnr snp rnp pns snr) (setf play (list '(-1 P))))
               ((>= ns r p s nr np rns pnr snp rnp pns snr) (setf play (list '(-1 S))))
               ((>= rns r p s nr np ns pnr snp rnp pns snr) (setf play (list '(-1 S) '(1 R))))
               ((>= pnr r p s nr np ns rns snp rnp pns snr) (setf play (list '(-1 R) '(1 P))))
               ((>= snp r p s nr np ns rns pnr rnp pns snr) (setf play (list '(-1 P) '(1 S))))
               ((>= rnp r p s nr np ns rns pnr snp pns snr) (setf play (list '(-1 P) '(1 R))))
               ((>= pns r p s nr np ns rns pnr snp rnp snr) (setf play (list '(-1 S) '(1 P))))
               ((>= snr r p s nr np ns rns pnr snp rnp pns) (setf play (list '(-1 R) '(1 S))))
               (t (setf play (list (random-agent))))))
              (t (setf play (list (random-agent)))))
          play))
      ;; MAIN - historical-agent
      (let ((sub1 (pop sub-perf)) perf1 play1 gplay1
            (sub2 (pop sub-perf)) perf2 play2 gplay2
            (sub4 (pop sub-perf)) perf4 play4 gplay4
            (sub8 (pop sub-perf)) perf8 play8 gplay8
            (recent (car hist)) pick base)
        ;; update the performance of each subroutine
        (if (not (null recent))
          (progn
            (setf sub1 (update-perf sub1 recent 8))
            (setf sub2 (update-perf sub2 recent 8))
            (setf sub4 (update-perf sub4 recent 8))
            (setf sub8 (update-perf sub8 recent 8))))
        (setf perf1 (car sub1)) (setf perf2 (car sub2))
        (setf perf4 (car sub4)) (setf perf8 (car sub8))
        ;; pick historical best choice
        (setf play1 (hist-helper 1 hist mem)) (setf gplay1 (guess play1 gn))
        (setf play2 (hist-helper 2 hist mem)) (setf gplay2 (guess play2 gn))
        (setf play4 (hist-helper 4 hist mem)) (setf gplay4 (guess play4 gn))
        (setf play8 (hist-helper 8 hist mem)) (setf gplay8 (guess play8 gn))
        ;; update subroutine plays
        (setf (cadr sub1) gplay1) (setf (cadr sub2) gplay2)
        (setf (cadr sub4) gplay4) (setf (cadr sub8) gplay8)
        ;; select subroutine based on past performance
        (cond
          ((>= perf8 perf4 perf2 perf1 1/100) (setf base play8) (setf pick gplay8))
          ((>= perf4 perf8 perf2 perf1 1/100) (setf base play4) (setf pick gplay4))
          ((>= perf2 perf4 perf8 perf1 1/100) (setf base play2) (setf pick gplay2))
          ((>= perf1 perf4 perf2 perf8 1/100) (setf base play1) (setf pick gplay1))
          (t (setf pick (random-agent)) (setf base (list pick))))
        ;; update semi-local variables
        (push sub8 sub-perf) (push sub2 sub-perf) (push sub4 sub-perf)
        (push sub1 sub-perf)
        (list pick base sub-perf)))
    ;;;;
    ;; MAIN
    (let (sign bet pick base (recent (car history)) winners lastplay
          (hist-perf (pop hist-info)) (hist-sub (pop hist-info))
          (hist-mem (pop hist-info)) hist-results histbase histplay
          (strat-sub (pop strat-info)) strat-results guess opp-score
          (retR (car *total-returns*)) (retP (cadr *total-returns*))
          (retS (caddr *total-returns*)) retplay)
      (if (not (null recent))
        (progn
          ;; update the performance of each meta-agent
          (setf lastplay (cadr hist-perf))
          (setf hist-perf (update-perf hist-perf recent 16))
          ;; update the performance of the strategic-agent
          (setf strat-results (attain-strat strat-sub recent))
          (setf strat-sub (pop strat-results)) (setf guess (pop strat-results))
          ;; get the winners of the previous round
          (setf winners (who-won recent)))
        ;; set the guess number to three if history is null
        (progn (setf guess (nth (random 4) '(4 3 2 1)))
          (setf lastplay (random-agent))))
      ;; call historical-agent
      (setf hist-results (historical-agent hist-sub hist-mem history guess))
      ;; update historical-memory
      (cond
        ((null hist-mem) (if (not (null history)) (push recent hist-mem)))
        ((< (length hist-mem) 2048) (if (not (null history)) (push recent hist-mem)))
        ((and (< (car hist-perf) 1/3) (> (length hist-mem)) 1536)
          (setf hist-mem (reverse hist-mem)) (dotimes (i 128) (pop hist-mem))
          (setf hist-mem (reverse hist-mem)) (push '() hist-mem))
        (t nil))
      ;; update historical-info
      (setf histplay (pop hist-results)) (setf histbase (pop hist-results))
      (setf hist-sub (pop hist-results))
      (setf (cadr hist-perf) histplay) (push hist-mem hist-info)
      (push hist-sub hist-info) (push hist-perf hist-info)
      ;; select agent based on past performance
      (setf base histbase) (setf sign (pop histplay)) (setf pick (pop histplay))
      ;; update strategic play
      (setf strat-sub (set-strat strat-sub base))
      ;; update strategic-info
      (push strat-sub strat-info)
      ;;;;
      ;; BET
      (setf opp-score (- (apply #'+ scores) myscore))
      ;;;;; DEBUG
;      (format t "myscore: ~1$~%" myscore)
      (if (and (> (car hist-perf) (nth (random 4) '(1/2 3/5 7/10 4/5))))
;            (= (feedback lastplay recent) 1))
        (cond
          ((> myscore 134197248) (setf bet (* sign 33549312)))
          ((> myscore 67098624) (setf bet (* sign 16774656)))
          ((> myscore 33549312) (setf bet (* sign 8387328)))
          ((> myscore 16774656) (setf bet (* sign 4193664)))
          ((> myscore 8387328) (setf bet (* sign 2096832)))
          ((> myscore 4193664) (setf bet (* sign 1048416)))
          ((> myscore 2096832) (setf bet (* sign 524208)))
          ((> myscore 1048416) (setf bet (* sign 262104)))
          ((> myscore 524208) (setf bet (* sign 131052)))
          ((> myscore 262104) (setf bet (* sign 65536)))
          ((> myscore 131052) (setf bet (* sign 32768)))
          ((> myscore 65536) (setf bet (* sign 16384)))         
          ((> myscore 32768) (setf bet (* sign 8192)))
          ((> myscore 16384) (setf bet (* sign 4096)))
          ((> myscore 8192) (setf bet (* sign 2048)))
          ((> myscore 4096) (setf bet (* sign 1024)))
          ((> myscore 2048) (setf bet (* sign 512)))
          ((> myscore 1024) (setf bet (* sign 256)))
          ((> myscore 512) (setf bet (* sign 128)))
          ((> myscore 256) (setf bet (* sign 64)))
          ((> myscore 128) (setf bet (* sign 32)))
          ((> myscore 64) (setf bet (* sign 16)))
          ((> myscore 32) (setf bet (* sign 8)))
          ((> myscore 16) (setf bet (* sign 4)))
          ((> myscore 8) (setf bet (* sign 3)))
          ((> myscore 4) (setf bet (* sign 2)))
          (t (setf bet (* sign 1))))
        (setf bet (* sign 1)))
      ;;;;
      ;; worry about *total-returns*
      (case pick
        (R (if (> retR (/ bet 2))
             (progn (setf retplay (random-pick retR retP retS))
               (setf bet (pop retplay)) (setf pick (pop retplay)))))
        (P (if (> retP (/ bet 2))
             (progn (setf retplay (random-pick retR retP retS))
               (setf bet (pop retplay)) (setf pick (pop retplay)))))
        (t (if (> retS (/ bet 2))
             (progn (setf retplay (random-pick retR retP retS))
               (setf bet (pop retplay)) (setf pick (pop retplay))))))
      ;;;;
      ;; return BET PICK
      (list bet pick))))
