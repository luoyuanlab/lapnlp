;;; -*- Mode: Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#

(defpackage :util
  (:use :common-lisp :regexp :excl)
  (:export "for-dir" "map-dir"
	   #+allegro "directorize"
	   #+allegro "create-dir"))

(in-package :util)

(defun for-dir (fn dir &key (recursive t) (do-dirs nil) &aux is-dir?)
  "Applies FN to every file in DIR.  If DO-DIRS is true (default false),
   also apply FN to directories within DIR.  If RECURSIVE is true (default),
   then apply FOR-DIR to each subdirectory."
  (labels 
      ((inner (d)
              (dolist (f (directory d 
                                    #+allegro :directories-are-files
                                    #+allegro nil))
                (setq is-dir? (file-directory-p f))
                (when (or do-dirs (not is-dir?))
                  (funcall fn f))
                (when (and recursive is-dir?)
                  (inner f)))))
    ;; If the initial directory was given without the following "\", then
    ;; it will seem to be a file (even though file-directory-p is true),
    ;; and we will find nothing.
    (if (file-directory-p dir)
        (inner 
         #+allegro
         (if (or (pathname-name dir) (pathname-type dir))
             (directorize dir)
           dir)
         #-allegro
         dir)
      (error "File specification ~s is not a directory." dir))))

(defun map-dir (fn dir &key (recursive t) (do-dirs nil))
  "Just like FOR-DIR, except the results are collected into a list."
  (let ((answer nil))
    (for-dir #'(lambda (f)
                 (push (funcall fn f) answer))
             dir
             :recursive recursive
             :do-dirs do-dirs)
    (nreverse answer)))


;; create dir with dirname, use recursion if necessary
#+allegro
(defun create-dir (dir)
  (let ((udir (replace-re dir "^(.*)(/|\\\\).+" "\\1")))
    (unless (probe-file udir)
      (create-dir udir))
    (unless (probe-file dir)
      (format t "creating dir ~a~%" dir)
      (excl.osi:mkdir dir))))

#+allegro
(defun directorize (dir)
  ;; dir is a pathname of a directory, but in the form of a file pathname.
  (let* ((n (pathname-name dir))
         (e (pathname-type dir))
         (dn (if e (concatenate 'string n "." e) n)))
    (make-pathname :defaults dir
                   :name nil
                   :type nil
                   :directory (append (pathname-directory dir) (list dn))))
  )
