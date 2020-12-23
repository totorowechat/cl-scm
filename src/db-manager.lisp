(defpackage #:cl-scm/db-manager
  (:use :cl)
  (:import-from #:cl-dbi)
  (:export #:delete-all-fileinfo
           #:insert-fileinfo
           #:select-fileinfo
           #:set-*db-path*  	       
           ))

(in-package #:cl-scm/db-manager)

;; ============================================================================
;; Sqlite
;; ============================================================================

(defparameter *db-path* nil)

(defun set-*db-path* (pathname)
	(setf *db-path* pathname))

(defun select-fileinfo ()
  "db-path: pathname"
	(dbi:with-connection (conn :sqlite3 :database-name *db-path*)
	  (let* ((query (dbi:prepare conn "SELECT * FROM Files"))
	         (query (dbi:execute query)))
	    (loop for row = (dbi:fetch query)
	          while row
	          do (format t "~A~%"  row)))))

; (defun insert-fileinfo (fileinfo-list)
;   ""
;   (dbi:with-connection (conn :sqlite3 :database-name *db-path*)
;   	(dbi:do-sql 
;   	  conn
;   	  "INSERT INTO Files
;       (title, f_type, f_path, f_size, accessed, modified, note, hash)
;       VALUES(?, ?, ?, ?, ?, ?, ?, ?);"
;   	  fileinfo-list)))

(defun insert-fileinfo (fileinfo-list)
  ""
  (dbi:with-connection (conn :sqlite3 :database-name *db-path*)
    (mapcar (lambda (x)
             (dbi:do-sql
              conn 
              "INSERT INTO Files
              (title, f_type, f_path, f_size, accessed, modified, note, hash)
              VALUES(?, ?, ?, ?, ?, ?, ?, ?);"
              x))
            fileinfo-list)))


(defun delete-all-fileinfo ()
  (dbi:with-connection (conn :sqlite3 :database-name *db-path*)
    (dbi:do-sql 
      conn
      "DELETE FROM Files;")))

;; ============================================================================
;; TODO Other database system
;; ============================================================================