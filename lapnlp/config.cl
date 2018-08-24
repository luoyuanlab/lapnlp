;;; -*- Mode: Lisp; Package: late -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework

LATE needs to access various external facilities that might be located
in different places on different types of computers, or even within a
machine/OS family. This file encapsulates all of these local file
system dependencies.

The overall idea is that for each such dependency, we identify the
right place in the file system by looking in the following sequence of
places.  If any of these is either not defined or does not identify an
existing file on the current machine, we continue down the list:

1. a preferences entry in a .late preferences file; we look for one in
the Lisp's home directory, and then also in the user's home directory.

2. a system environment variable that has been set to point to the
right place in the file system to look for a file or set of files

3. a logical pathname translation that identifies the right place

4. defaults based on the specific host name; however, this is
difficult for portable machines, because the host name varies 
depending on how it gets its DHCP address

5. defaults based on the machine and system type of the Lisp intallation

6. defaults in case none of the above applied.

|#

(defpackage :late
  (:use :common-lisp :util #+allegro :excl)
  (:export "*defaults-by-host*"
	   "get-by-host"
	   "*defaults-by-system*"
	   "get-by-system"
	   "*defaults-general*"
	   "get-general"
	   "logical-host-exists"
	   "get-env"
	   "get-envs"
	   "get-env-pathname"
	   "get-directory-pathname"
	   "pathdir"
	   "make-path"
	   "*late-prefs*"
	   "read-prefs"))

(in-package :late)

(defparameter *late-prefs* (make-hash-table :test #'equalp))

(defparameter *defaults-by-host*
  '(((<"late deployment hostnames having the following config">)
     ("WEKAHOME" . "<weka home directory>")
     ("UMLS_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")
		   ("<hostname2>" "<db name2>" "<user2>" "<pwd2>")))
     ("LATE_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")))
     ("WEKA_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")))
     ("GAZETTE_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>"))))
    ;; the following is an example
    (("someserver.someschool.edu" "someserver")
     ("UMLS_DB" . (("localhost" "2017ab" "user" "pwd")))
     ("LATE_DB" . (("localhost" "late" "user" "pwd")))
     ("WEKA_DB" . (("localhost" "weka" "user" "pwd")))
     ("GAZETTE_DB" . (("localhost" "gazettes" "user" "pwd")))))
  "This data structure lists pathnames specific to various environment
    variables that we need to set.  Its value is a list of
    associations. Each association's first element is a list of machine
    names (as returned by (short-site-name)).  The rest is an
    association list that matches environment names to their pathname
    values.")

(defun get-by-host (env-var)
  "Looks up a definition of env-var depending on the current
  host. Because short-site-name varies depending on where a machine is
  connected, we allow this to be specified by the shell variable
  MACHINE, which takes precedence."
  (let ((hostname (or (system:getenv "MACHINE")
		      (short-site-name)))
	(ans nil))
    (dolist (ha *defaults-by-host* nil)
      (when (or (eq hostname (car ha))
		(and (consp (car ha))
		     (member hostname (car ha)
			     :test #'equal)))
	(when (setq ans (assoc env-var (cdr ha)
			       :test #'equal))
	  (return (cdr ans)))))))

(defparameter *defaults-by-system*
  '((:macosx
     ("JAVA_HOME" . "<java home no mac>")
     ("JNI_OFFSET" . "<home of franz' java native interface on mac>")
     ("LVG_DIR" . "<lvg home on mac>")
     ("WEKAHOME" . "<weka home on mac>"))
    (:mswindows
     ("JAVA_HOME" . "<java home on windows>")
     ("JNI_OFFSET" . "<home of franz' java native interface on windows>"))
    (:linux
     ("JAVA_HOME" . "<java home on linux>")
     ("JNI_OFFSET" . "<home of franz' java native interface on linux>"))
    )
  "An association list that allows LATE to look up defaults depending
    on the type of system being run, among :mswindows, :macosx,
    :linux, or any other keyword appearing in *features*.")

(defun get-by-system (env-var &aux (temp nil))
  (dolist (sa *defaults-by-system* nil)
    (when (featurep (car sa))
      (when (setq temp (assoc env-var (cdr sa) :test #'equal))
	(return (cdr temp))))))

(defun pathdir (path)
  "Extracts the dir of a path."
  (make-pathname :directory (pathname-directory path)))

(defparameter *defaults-general*
  '(("JLINKER_JAR" . "sys:;jlinker;jlinker.jar")
    ("JLINKER_HOME" . "sys:;jlinker;")
    ("OPENNLP_HOME" . "opennlp:;")
    ("UMLS_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")
		  ("<hostname2>" "<db name2>" "<user2>" "<pwd2>")))
    ("LATE_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")))
    ("WORDNET_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")))
    ("GAZETTE_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>")))
    ("GEOG_DB" . (("<hostname>" "<db name>" "<user>" "<pwd>"))))
  "An association list that allows LATE to look up defaults for
    various items if they have not already been found by earlier
    means.")

(defun get-general (env-var)
  (let ((temp (assoc env-var *defaults-general* :test #'equal)))
    (and temp (cdr temp))))

(defun logical-host-exists (hostname)
  "Determines if the hostname is defined as a logcial host, and returns
   a pathname to it if it does."
  (ignore-errors
    (load-logical-pathname-translations hostname)
    (make-pathname :host hostname)))

(defun get-env (env-var)
  "Gets a default value for some environment variable, using the
  cascade of successive defaults defined above, except for 2, logical
  pathname translations. Generally, this is for variables that do not
  name pathnames in the file system; for those, use get-env-pathname."
  (or (gethash env-var *late-prefs*)
      (system:getenv env-var)
      (get-by-host env-var)
      (get-by-system env-var)
      (get-general env-var)))

(defun get-envs (env-var)
  "Gets a list of potential values for some environment variable,
  appending together values (which must be lists) retrieved from
  different default sources."
  (let ((prefval (gethash env-var *late-prefs*))
	(envval (system:getenv env-var)))
    (append (if (listp prefval) prefval (list prefval))
	    (and envval (list envval))
	    (get-by-host env-var)
	    (get-by-system env-var)
	    (get-general env-var))))

(defun get-env-pathname (env-var &optional (logical-hostname env-var))
  "Gets a default pathname for some environment variable, using the
  cascade of successive defaults defined above. This is like get-env,
  except that when we find a value, we make sure that it identifies an
  existing pathname in the file system."
  (let ((place nil)
	(prefval (gethash env-var *late-prefs*)))
    (or (dolist (pl (if (listp prefval) prefval (list prefval)))
	  (let ((f (probe-file pl)))
	    (when f (return-from get-env-pathname f))))
	(and (setq place (system:getenv env-var))
	     (probe-file place))
	(logical-host-exists logical-hostname)
	(and (setq place (get-by-host env-var))
	     (probe-file place))
	(and (setq place (get-by-system env-var))
	     (probe-file place))
	(and (setq place (get-general env-var))
	     (probe-file place)))))

(defun get-directory-pathname (pn)
  "Strings that name directories but do not end in '/' (in Unix
  systems) are interpreted as if they were file names instead, leading
  to incorrect combinations with incremental file system names. This
  function assures that a string or pathname of that sort is treated
  as a directory iff such a directory exists in the file system."
  (and pn 
       (let* ((n (pathname-name pn))
	      (e (pathname-type pn))
	      (dn (if e (concatenate 'string n "." e) n)))
	 (if (and n (file-directory-p pn))
	     (make-pathname :defaults pn
			    :name nil
			    :type nil
			    :directory (append (pathname-directory pn)
					       (list dn)))
	   (pathname pn)))))

(defun make-path (relative-pathname base-string)
  "Creates a path from the base and relative, paying attention to the
  problem that the base might not look like a directory."
  (let ((base (get-directory-pathname base-string)))
    (and base (merge-pathnames relative-pathname base))))

(defun read-prefs (&optional (show nil))
  "Reads system and user init files for late. System files are in sys:;, user
   files in ~/.  File names are either .late or late.pref; only the first of these
   found is read, but both system and user init files are read if both exist. This
   assures that user initializations override system ones. 
   An empty value is saved as nil. This is useful to eliminate a system default.
   Each preference begins with a symbol (any string not including white space) in
   the first character position of a line. The value associated with that symbol
   starts beyond the white space ending the symbol and continues until the next
   line that defines a new symbol.  I.e., continuation lines that begin with white
   space are concatenated to the starting line.
   Lines that begin (in col 1) with # and blank lines are ignored.
   A value that begins with ( or #( is read by the Lisp reader.
   The results are stored in *late-prefs*."
  (clrhash *late-prefs*)
  (labels ((record (p v &aux (err (list 'err)))
		   (when p
		     (setf (gethash p *late-prefs*)
			   (cond ((string= v "") nil)
				 ((or (char= #\( (char v 0))
				      (and (> (length v) 1)
					   (char= #\# (char v 0))
					   (char= #\( (char v 1))))
				  (multiple-value-bind (vv n) (read-from-string v nil err)
				    (cond ((and (not (eq vv err)) (>= (1+ n) (length v)))
					   vv)
					  (t (format t "~%;;; Warning! Malformed pref ~s = ~s" p v)
					     nil))))
				 (t v))))))
    (dolist (place (list "sys:" (user-homedir-pathname)))
      ;; iterate over both system and user possible init file locations
      (dolist (pfn '(".late" "late.pref"))
	;; iterate over possible init file names; only read the first one found
	(let ((initfile (probe-file (merge-pathnames pfn place))))
	  (when initfile
	    (format t "~%;;; Reading LATE initialization file ~a" initfile)
	    (do ((lines (read-lines initfile) (cdr lines))
		 (pref) (prop nil) (val ""))
		((null lines)
		 (record prop val))
	      (setq pref (car lines))
	      (if (not (or (zerop (length pref)) (char= (char pref 0) #\#)))
		  ;; i.e., not blank or comment line
		  (multiple-value-bind (m? whole key str)
		      (match-re "^([^\\s]+)\\s*(.*)$" pref)
		    (declare (ignore whole))
		    (cond (m?
			   (record prop val)
			   (setq prop key val str))
			  (t
			   (setq val (concatenate 'string val (string #\Newline) pref)))))))
	    (return nil))))))
  (when show
    (maphash #'(lambda (k v)
		 (format t "~%;;; ~a ~12,4T~s" k v))
	     *late-prefs*))
  (hash-table-count *late-prefs*))
