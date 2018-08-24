;;; -*- Mode: Lisp; Package: nihr -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 03/10/2017 rewrite using asdf framework
yluo - 10/08/2015 creation
|#
(defpackage :nihr
  (:use :late :umlsize :excl :excl.osi :common-lisp :util :norm)
  (:export
   "mesh-title"
   ))

(in-package :nihr)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

(defmemo str->cui-clean (str &key (cvf nil))
  "Another config: cvf = *cvf-nlp*
In order to use this function, you need to augment lragr as follow:
alter table MRCONSO add RSTR TEXT;
update MRCONSO set rstr = replace(str, '-', ''); 
update MRCONSO set rstr = replace(rstr, '.', '');
alter table MRCONSO add index MRCONSO_RSTR (RSTR(255));"
  
  (let*  ((old-str str)
	  ans max-cnt)
    (open-umls)
    (setf str (replace-re str "\\s+" " "))
    (setf str (replace-re str "[.-]" "")) ;; \\-
    ;; (lragr-base str)
    (setf ans (umlssql "SELECT CUI, COUNT(*) FROM MRCONSO WHERE RSTR~a ~@[and CVF & ~a~] GROUP BY CUI ORDER BY COUNT(*) DESC"
		       (sql-matcher str) cvf))
    (setf max-cnt (second (first ans)))
    (setf ans (remove-if #'(lambda (x) (< (second x) max-cnt)) ans))
    (setf ans (mapcar #'car ans))
    (when (and (not ans)
	       (match-re "(?i)^[a-z()\\-\\s]+$" old-str))
      (setf ans (norm->cui (norm:norm old-str))))
    ans))

(defun mesh-title (title
		   &key (n-tokens 6)) ;; *default-max-concept-length*
  (let* ((tokens (split-re "\\s+" title))
	 t-meshs)
    (cond ((null tokens)
	   (format t "~&Warning: Empty token title: ~a" title))
	  (t
	   (let* ((n (length tokens))
		  (interpretations		;as CUIs
		   (make-array (list n n) :initial-element nil))
		  (m-sp-poses
		   (make-array (list n n) :initial-element nil)))
	     ;; We will be filling in the n-tokens diagonal and superdiagonal
	     ;; elements of the interpretations array with lists of UMLS CUIs
	     ;; retrieved for subsequences of tokens from the sentence. The
	     ;; (i, j) element of this matrix shows the CUIs retrieved for a
	     ;; normalized form of the text spanning from the start of the i'th
	     ;; to the end of the j'th token. We will call such a subsequence
	     ;; of tokens a span.
	     ;; Because lp's tokenizer includes tokens for punctuation, we will
	     ;; often retrieve the same list of CUIs for different spans.  In
	     ;; this case, we want to assign them only to the shortest span for
	     ;; which they appear. Therefore, we explore the set of spans from
	     ;; shortest to longest, and for each subspan encompassed by a
	     ;; longer span, we omit those CUIs for the longer span that
	     ;; already occur in the subspan.
	     ;; We use Lisp's set-difference to do these computations, which
	     ;; could be made more efficient by keeping the CUIs in sorted
	     ;; order. We also use an n x n array for interpretations, although
	     ;; only the diagonal and some superdiagonals are populated.  We
	     ;; could save space by a more sophisticated data structure design.
	     ;; We also eliminate CUI interpretations of single tokens that
	     ;; fall into the function-word grammatical categories, because the
	     ;; CUI we look up for these is often misleading. For example, "is"
	     ;; normalizes to "be", which has the CUI for Beryllium associated
	     ;; with it.
	     ;; 
	     (dotimes (len n-tokens)
	       (do ((i 0 (1+ i))
		    (j 0))
		   ()
		 (when (>= (setq j (+ i len)) n) (return t))
		 (let* ((text (format nil "~{~a~^ ~}" (subseq tokens i (1+ j))))
			(cuis (str->cui-clean text))
			(sp-poses (hstr->sp-pos text))
			)
		   ;; cuis now holds all CUIs for the span from i to j
		   ;; yluo is changing, well ... back for now 
		   ;; (when cuis
		   ;; 	 ;; (format t "~%(~d, ~d): ~a" i j cuis)
		   ;; 	 ;; remove the ones already present in subspans
		   ;; 	 (do ((k i (1+ k)))
		   ;; 		 ((or (null cuis) (> k j)))
		   ;; 	   (do ((l k (1+ l)))
		   ;; 		   ((or (null cuis) (> l j)))
		   ;; 		 (unless (and (= k i) (= l j))
		   ;; 		   ;; don't consider (i, j) to be a subspan of itself!
		   ;; 		   (let ((subcuis (aref interpretations k l)))
		   ;; 			 (when subcuis
		   ;; 			   (setq cuis (set-difference cuis subcuis :test #'equal))))))))
		   ;; (setf cuis (or cuis str-cuis))
		   (when cuis
		     ;; Note that it's important to store these even if we
		     ;; flush them later for words being too short; otherwise,
		     ;; supersets of this span might get those CUIs.
		     (setf (aref interpretations i j) cuis))
		   (when sp-poses
		     (setf (aref m-sp-poses i j) sp-poses)))))
	     (dotimes (i n)
	       (let* ((sp-pos (aref m-sp-poses i i)))
		 
		 (when
		     ;; Now get rid of CUIs for single tokens that are function
		     ;; words of the language (i.e., closed categories)
		     ;; Note that many of the function words in Specialist also
		     ;; have other syntactic categories.  E.g., "was" can be
		     ;; either an aux (expected) or a noun (puzzling). We
		     ;; suspect that the latter kinds of cases are almost never
		     ;; useful, so we treat any word any of whose syntactic
		     ;; categories is a function type as such.  
		     (intersection sp-pos *sp-lex-fn-scas* :test #'equal)
		   (setf (aref interpretations i i) nil))
		 (do ((j i (1+ j)))
		     ((or (>= j (+ i n-tokens)) (>= j n)))
		   (let* ((cuis (aref interpretations i j))
			  ;; yluo, hack to incorporate domain specific semantic
			  ;; type
			  (meshs (or (cui->mesh cuis) 
				     (cui->mesh-supp cuis))) )
		     (setf t-meshs (union t-meshs meshs)))))))))
    t-meshs))
