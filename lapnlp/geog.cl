;;; -*- Mode: Lisp; Package: util; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
This is utility code to load the city and street name data from Tiger
2000, provided to me by Lee Hetherington. The data as I got them are
organized in a very simple form:
1. The cities directory contains one file for each state, holding a list
of city names, one per line.
2. The streets directory contains one subdirectory per state; each of
those has a file per city, holding the list of street names in that city.
I presume that the cities data is redundant with the state data, but
will need to check.
Each street entry contains the spelled-out name of the type of street
(Avenue, Road, Boulevard, etc.).  I will need to make a catalog of these.
All data are in lower-case.

I have created a database on Maimonides called geog, to hold these
data. Initially, this is simply in a single table:
create table streets(
       id integer not null auto_increment primary key,
       state varchar(255) not null,
       city varchar(255) not null,
       street varchar(255) not null,
       key (state),
       key (city),
       key (street))
       ENGINE=MyISAM DEFAULT CHARSET=UTF8;

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mysql))

(defpackage :util
  (:use :common-lisp :late)
  (:export "load-streets"))

(in-package :util)

(defun load-streets (&optional (pn "<folder for streets file>") (geog-db nil))
  (let ((city-files (directory pn)))
    (dolist (cf city-files)
      (let* ((state (sqs (car (last (pathname-directory cf)))))
	     (city (sqs (pathname-name cf)))
	     (streets (read-csv cf)))
	(unwind-protect
	    (progn
	      (setq geog-db (dbi.mysql:connect :host "<hostname>"
					       :database "<db name>"
					       :user "<user>"
					       :password "<pwd>"
					       :port 3306))
	      (dolist (ss streets)
		(dbi.mysql:sql (format nil
				       "insert into streets(state, city, street) values(~a,~a,~a)"
				       state city (sqs (car ss)))
			       :db geog-db)))
	  (when geog-db
	    (dbi.mysql:disconnect :db geog-db)))))))
