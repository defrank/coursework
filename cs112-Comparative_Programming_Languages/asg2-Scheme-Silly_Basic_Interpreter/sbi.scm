#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket-5.1/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.1 2011-04-26 09:14:43-07 dmfrank - $
;; Derek Frank, dmfrank@ucsc.edu
;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an
;;    SBIR program, which is the executed.  Currently it is only
;;    printed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; *stderr*
;;
(define *stderr* (current-error-port))

;;
;; *run-file*
;;
(define *run-file*
  (let-values
      (((dirpath basepath root?)
        (split-path (find-system-path 'run-file))))
    (path->string basepath))
)

;;
;; die
;;
(define (die list)
  (for-each (lambda (item) (display item *stderr*)) list)
  (newline *stderr*)
  (exit 1)
)

;;
;; usage-exit
;;
(define (usage-exit)
  (die `("Usage: " ,*run-file* " filename"))
)

;;
;; show
;;    Prints to stdout two arguments usually from a hash table
;;
(define (show label item)
  (newline)
  (display label) (display " = ") (display item)
  (newline)
)

;;
;; labels-hash
;;    A global hash table that will store a label as a key and its
;;    corresponding statement as its value.
;;
(define labels-hash (make-hash))

;;
;; vars-hash
;;    A global hash table that will store a variable as a key and its
;;    corresponding value as its value.
;;
(define vars-hash (make-hash))

;;
;; scan-labels
;;    Scans a given program and stores the labels as keys with the
;;    statements being the values.
;;
(define (scan-labels program)
  (map (lambda (line)
         (when (and (not (null? line)) (>= (length line) 2)
                    (not (null? (cdr line))) (symbol? (cadr line)))
               (hash-set! labels-hash (cadr line) (caddr line))))
       program)
)

;;
;; sbir-dim
;;    Creates an array.
;;
(define (sbir-dim args)
  (if (or (null? args) (not (= (length args) 2)))
      (die '("invalid array operands"))
      (let* ((name (car args))
             (dimen (cadr args))
             (vec (make-vector dimen)))
        (hash-set! vars-hash name vec)))
)

;;
;; sbir-let
;;    Creates variables.
;;
;(define (sbir-let args)
  
;)

;;
;; sbir-goto
;;    Alters current place of expression execution by jumping.
;;
;(define (sbir-goto args)
  
;)

;;
;; sbir-if
;;    A conditional statement.  Can make jumps.
;;
;(define (sbir-if args)
  
;)

;;
;; sbir-print
;;    Prints its argument with a carriage return.
(define (sbir-print args)
  (when (not (null? args))
        (map (lambda (item)
               ((if (char=? (string-ref item 0) #\") 
                    (let ((str (substring item 1 (- (length item) 2))))
                      (display str))
                    (display (hash-ref vars-hash item #f)))))
             args))

  (newline)
)

;;
;; sbir-input
;;
;(define (sbir-input args)
  
;)

;;
;; readlist-from-inputfile
;;
(define (readlist-from-inputfile filename)
  (let ((inputfile (open-input-file filename)))
    (if (not (input-port? inputfile))
        (die `(,*run-file* ": " ,filename ": open failed"))
        (let ((program (read inputfile)))
          (close-input-port inputfile)
          program)))
)

;;
;; interpret-program
;;
(define (interpret-program filename program)

  ;; Preliminary scan for labels to be placed into hash table
  (scan-labels program)

  (map (lambda (line) 
         (let* ((line-len (length line))
                (last-index (- line-len 1))
                (last-elem (list-ref line last-index)))
           (when (and (>= line-len 2) (not (symbol? last-elem))
                      (not (null? last-elem)))
                 (let ((fn (car last-elem))
                       (stmt (if (> (length last-elem) 1) 
                                 (cdr last-elem)
                                 ( '() ))))
                   (cond ((equal? fn 'dim) (sbir-dim stmt))
;                         ((equal? fn ,let) (sbir-let stmt))
 ;                        ((equal? fn ,goto) (sbir-goto stmt))
  ;                       ((equal? fn ,if) (sbir-if stmt))
                         ((equal? fn 'print) (sbir-print stmt))
   ;                      ((equal? fn ,input) (sbir-input stmt))
                         (else (die '("invalid expression"))))))))
       program)
)

;;
;; main
;;
(define (main arglist)
  (if (or (null? arglist) (not (null? (cdr arglist))))
      (usage-exit)
      (let* ((sbprogfile (car arglist))
             (program (readlist-from-inputfile sbprogfile)))
        (interpret-program sbprogfile program)))
)

;;
;; BEGIN MAIN
;;
(main (vector->list (current-command-line-arguments)))








;  (printf "==================================================~n")
;  (printf "~a: ~s~n" *run-file* filename)
;  (printf "==================================================~n")
;  (printf "(~n")
;  (map (lambda (line) (printf "~s~n" line)) program)
;  (printf ")~n")
;  (newline) (newline)





  ;; DEGUB: prints labels-hash table to stdout
;  (hash-for-each labels-hash
 ;                (lambda (key value) (show key value)))


