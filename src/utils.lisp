(defpackage #:cl-scm/utils
  (:use :cl)
  (:export ;; dates and time
           #:unix-to-universal-time
  	       #:strftime

           ;; hash
           #:md5-hash-from-pathname
           #:md5-hash-from-string

           ;; file
           #:file-size-from-stream
           ))


(in-package #:cl-scm/utils)

;; ============================================================================
;; Dates and Time
;; ============================================================================

;; Unix Time

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

;; Time Formatting

(defvar *day-names*
           '("Monday" "Tuesday" "Wednesday"
	         "Thursday" "Friday" "Saturday"
	         "Sunday"))

(defun strftime (encoded-time)
	(multiple-value-bind
	           (second minute hour day month year day-of-week dst-p tz)
	    	   (decode-universal-time encoded-time)
	           (format nil "It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
		    	 hour
		    	 minute
		    	 second
		    	 (nth day-of-week *day-names*)
		    	 month
		    	 day
		    	 year
		    	 (- tz))))

;; ============================================================================
;; Hash
;; ============================================================================

(defun md5-hash-from-string (s)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence
    :md5
    (trivial-utf-8:string-to-utf-8-bytes s))))

(defun md5-hash-from-pathname (p)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-file :md5 p)))

;; ============================================================================
;; File
;; ============================================================================

; This function is from ruricolist/trivial-file-size/trivial-file-size.lisp
(defun file-size-from-stream (file)
  "file: a pathname designator."
  (with-open-file (in file
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (file-length in)))