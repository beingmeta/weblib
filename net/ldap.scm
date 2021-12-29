;; -*- Mode: Scheme; Character-encoding: utf-8; -*-
;;; Copyright (C) 2010-2020 beingmeta, inc. All rights reserved
;;; Copyright (C) 2020-2021 beingmeta, llc.

(in-module 'net/ldap)

(use-module '{webtools curl reflection varconfig opts})
(use-module '{texttools logger})
(define %used_modules '{varconfig})

(define-init ldap-host "localhost")
(define-init ldap-port 389)
(define-init ldap-root #f)

(define-init default-ldap-scope "base")

(module-export! '{domain->dn ldap-url ldap/search})

(define (domain->dn domain)
  (stringout
    (doseq (name (textslice domain "." #f) i)
      (printout (if (> i 0) ",") (printout "dc=" name)))))
							  
(define (attribs->clist attribs)
  (cond ((ambiguous? attribs)
	 (stringout (do-choices (elt attribs i) (printout (if (> i 0) ",") elt))))
	((string? attribs) attribs)
	((symbol? attribs) (symbol->string attribs))
	((sequence? attribs)
	 (stringout (doseq (elt attribs i) (printout (if (> i 0) ",") elt))))
	(else "")))

(define (parse-dn string)
  (let* ((at (position #\@ string))
	 (head (and at (slice string 0 at))))
    (if at
	(glom (if (position #\= head) head )
	  ","
	  (domain->dn (slice string (1+ at))))
	(if (and (position #\. string)
		 (not (or (position #\= string) (position #\, string))))
	    (domain->dn string)
	    string))))

(define (->filter arg)
  (stringout (do-choices (each arg i)
	       (if (> i 0) (printout ","))
	       (cond ((string? arg) (printout each))
		     ((table? arg)
		      (do-choices (key (getkeys arg) i)
			))))))

(defambda (ldap-url host port base attribs scope filter)
  (glom "ldap://" host (and port ":") port "/" (parse-dn base)
    "?" attribs "?" scope (and filter "?") (and filter (->filter filter))))

(define (spec->hostname s)
  (if (position #\: s)
      (slice s 0 (position #\: s))
      s))
(define (spec->port s)
  (if (position #\: s)
      (slice s (1+ (position #\: s)) )
      389))

(define (ldap/search opts filter)
  (define host-arg (getopt opts 'host ldap-host))
  (let ((url (ldap-url (spec->hostname host-arg) 
		       (getopt opts 'port (spec->port host-arg))
		       (if (testopt opts 'base)
			   (parse-dn (getopt opts 'base))
			   (if (testopt opts 'dn)
			       (parse-dn (getopt opts 'dn))
			       ""))
		       (getopt opts 'attribs "")
		       (getopt opts 'scope "sub")
		       filter)))
    (urlget url)))

