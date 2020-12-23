(defpackage #:cl-scm/file
  (:use :cl)
  (:export #:fileinfo-struct-to-list
  	       #:get-attributes
  	       #:make-fileinfo-from-pathname
           #:scan
           #:walk-directory))

(in-package #:cl-scm/file)

;; ============================================================================
;; File Info
;; ============================================================================


(defstruct fileinfo
	(title "" :type string)
  (type  "" :type string)
	(path #P"" :type pathname)
	(size 0 :type integer)
	(accessed 0 :type integer)
	(modified 0 :type integer)
	(note "" :type string)
	(hash (make-array 32 :element-type 'base-char) :type simple-base-string))

(defun get-attributes (p fn)
	(let ((stat (osicat-posix:stat p)))
  		(funcall fn stat)))

; create struct fileinfo by pathname.
#-windows 
(defun make-fileinfo-from-pathname (p &optional (hash? nil))
	(make-fileinfo 
		:title (pathname-name p)
    :type (pathname-type p)
	  :path p
	  :size (get-attributes p #'osicat-posix:stat-size)
	  :accessed (get-attributes p #'osicat-posix:stat-atime)
	  :modified (get-attributes p #'osicat-posix:stat-mtime)
	  :hash (if hash? 
              (cl-scm/utils:md5-hash-from-pathname p)
              (make-array 32 :element-type 'base-char))))

#+windows
(defun make-fileinfo-from-pathname (p &optional (hash? nil))
  (make-fileinfo 
    :title (pathname-name p)
    :type (or (pathname-type p) "")
    :path p
    ; :size (cl-scm/utils:file-size-from-stream p)
    ; :accessed 
    ; :modified 
    :hash (if hash? 
              (cl-scm/utils:md5-hash-from-pathname p)
              (make-array 32 :element-type 'base-char))))

; *From*
; #S(CL-SCM/FILE::FILEINFO
;     :TITLE "Firefox Installer"
;     :TYPE "exe"
;     :PATH #P"/Data/Firefox Installer.exe"
;     :SIZE 334040
;     :ACCESSED 1608643882
;     :MODIFIED 1608176585
;     :NOTE ""
;     :HASH "59ea4645f2c6fea6eedeae79ea9a9885")
; *To*
; `(TITLE TYPE PATH SIZE ACCESSED MODIFIED NOTE HASH)
; '("Firefox Installer" 
;   "exe"
;   #P"/Data/Firefox Installer.exe" 
;   334040 1608643882 1608176585 
;   "" 
;   "59ea4645f2c6fea6eedeae79ea9a9885") 
(defun fileinfo-struct-to-list (s)
  "convert fileinfo from struct to list.
   notice: path is convert from pathname to string"
  (list (fileinfo-title s)
        (fileinfo-type s)
        (namestring (fileinfo-path s))
        (fileinfo-size s)
        (fileinfo-accessed s)
        (fileinfo-modified s)
        (fileinfo-note s)
        (fileinfo-hash s)))

;; ============================================================================
;; File Scan
;; ============================================================================

(defparameter *home-dir* #P"./")
(defparameter *file-list* nil)

(defun walk-directory (p &optional (file-list nil))
	"read all files in <p> recursively, exclude directories."
	(cl-fad:walk-directory p 
		(lambda (name)
			(push name file-list)) :directories nil)
	file-list)

(defun set-*file-list* (l)
	(setf *file-list* l))

(defun scan (p)
	(arrow-macros:->> p
		(walk-directory)
		(set-*file-list*)
		(map 'list #'make-fileinfo-from-pathname)
    (map 'list #'fileinfo-struct-to-list)))
